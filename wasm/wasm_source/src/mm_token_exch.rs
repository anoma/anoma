use std::collections::{HashMap, VecDeque};

use anoma_vm_env::matchmaker_prelude::intent::{
    Exchange, FungibleTokenIntent, MatchedExchanges,
};
use anoma_vm_env::matchmaker_prelude::key::ed25519::Signed;
use anoma_vm_env::matchmaker_prelude::{token, *};
use good_lp::{
    constraint, default_solver, variable, variables, Expression,
    ResolutionError, SolverModel, Variable, VariableDefinition,
};
use petgraph::graph::{node_index, DiGraph, NodeIndex};
use petgraph::visit::{depth_first_search, Control, DfsEvent, EdgeRef};
use petgraph::Graph;
use rust_decimal::prelude::*;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
struct ExchangeNode {
    id: Vec<u8>,
    exchange: Signed<Exchange>,
    intent: Signed<FungibleTokenIntent>,
}

impl PartialEq for ExchangeNode {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

#[matchmaker]
fn add_intent(graph_bytes: Vec<u8>, id: Vec<u8>, data: Vec<u8>) -> bool {
    let intent = decode_intent_data(&data);
    let exchanges = intent.data.exchange.clone();

    let mut graph = decode_graph(graph_bytes);
    log_string("trying to match new intent".to_string());
    exchanges.into_iter().for_each(|exchange| {
        add_node(&mut graph, id.clone(), exchange, intent.clone())
    });
    find_match_and_remove_node(&mut graph);
    update_graph_data(&graph);
    true
}

fn create_transfer(
    from_node: &ExchangeNode,
    to_node: &ExchangeNode,
    amount: token::Amount,
) -> token::Transfer {
    token::Transfer {
        source: from_node.exchange.data.addr.clone(),
        target: to_node.exchange.data.addr.clone(),
        token: to_node.exchange.data.token_buy.clone(),
        amount,
    }
}

fn send_tx(tx_data: MatchedExchanges) {
    let tx_data_bytes = tx_data.try_to_vec().unwrap();
    send_match(tx_data_bytes);
}

fn decode_intent_data(bytes: &[u8]) -> Signed<FungibleTokenIntent> {
    Signed::<FungibleTokenIntent>::try_from_slice(bytes).unwrap()
}

fn decode_graph(bytes: Vec<u8>) -> DiGraph<ExchangeNode, Address> {
    if bytes.is_empty() {
        Graph::new()
    } else {
        serde_json::from_slice(&bytes[..]).expect("error in json format")
    }
}

fn update_graph_data(graph: &DiGraph<ExchangeNode, Address>) {
    update_state(serde_json::to_vec(graph).unwrap());
}

fn find_to_update_node(
    graph: &DiGraph<ExchangeNode, Address>,
    new_node: &ExchangeNode,
) -> (Vec<NodeIndex>, Vec<NodeIndex>) {
    let start = node_index(0);
    let mut connect_sell = Vec::new();
    let mut connect_buy = Vec::new();
    depth_first_search(graph, Some(start), |event| {
        if let DfsEvent::Discover(index, _time) = event {
            let current_node = &graph[index];
            if new_node.exchange.data.token_sell
                == current_node.exchange.data.token_buy
            // && new_node.exchange.data.max_sell
            //     >= current_node.exchange.data.min_buy
            {
                connect_sell.push(index);
            }
            if new_node.exchange.data.token_buy
                == current_node.exchange.data.token_sell
            // && current_node.exchange.data.max_sell
            //     >= new_node.exchange.data.min_buy
            {
                connect_buy.push(index);
            }
        }
        Control::<()>::Continue
    });
    (connect_sell, connect_buy)
}

fn add_node(
    graph: &mut DiGraph<ExchangeNode, Address>,
    id: Vec<u8>,
    exchange: Signed<Exchange>,
    intent: Signed<FungibleTokenIntent>,
) {
    let new_node = ExchangeNode {
        id,
        exchange,
        intent,
    };
    let new_node_index = graph.add_node(new_node.clone());
    let (connect_sell, connect_buy) = find_to_update_node(graph, &new_node);
    let sell_edge = new_node.exchange.data.token_sell;
    let buy_edge = new_node.exchange.data.token_buy;
    for node_index in connect_sell {
        graph.update_edge(new_node_index, node_index, sell_edge.clone());
    }
    for node_index in connect_buy {
        graph.update_edge(node_index, new_node_index, buy_edge.clone());
    }
}

fn create_and_send_tx_data(
    graph: &DiGraph<ExchangeNode, Address>,
    cycle_intents: Vec<NodeIndex>,
) {
    log_string(format!(
        "found match; creating tx with {:?} nodes",
        cycle_intents.len()
    ));
    let cycle_intents = sort_cycle(graph, cycle_intents);
    let amounts = compute_amounts(graph, &cycle_intents);

    match amounts {
        Ok(res) => {
            log_string(format!(
                "amounts: {}",
                res.values()
                    .map(|x| x.to_string())
                    .collect::<Vec<String>>()
                    .join(", ")
            ));
            let mut cycle_intents_iter = cycle_intents.into_iter();
            let first_node =
                cycle_intents_iter.next().map(|i| &graph[i]).unwrap();
            let mut tx_data = MatchedExchanges::empty();

            let last_node = cycle_intents_iter.fold(
                first_node,
                |prev_node, intent_index| {
                    let node = &graph[intent_index];
                    let exchanged_amount =
                        *res.get(&node.exchange.data).unwrap();
                    log_string(format!(
                        "crafting transfer: {}, {}, {}",
                        node.exchange.data.addr.clone(),
                        prev_node.exchange.data.addr.clone(),
                        exchanged_amount
                    ));
                    tx_data.transfers.insert(create_transfer(
                        node,
                        prev_node,
                        exchanged_amount, /* safe as we have as many amounts
                                           * as intents */
                    ));
                    tx_data.exchanges.insert(
                        node.exchange.data.addr.clone(),
                        node.exchange.clone(),
                    );
                    tx_data.intents.insert(
                        node.exchange.data.addr.clone(),
                        node.intent.clone(),
                    );
                    node
                },
            );
            let last_amount = *res.get(&first_node.exchange.data).unwrap();
            log_string(format!(
                "crafting transfer: {}, {}, {}",
                first_node.exchange.data.addr.clone(),
                last_node.exchange.data.addr.clone(),
                last_amount
            ));
            tx_data.transfers.insert(create_transfer(
                first_node,
                last_node,
                last_amount,
            ));
            tx_data.exchanges.insert(
                first_node.exchange.data.addr.clone(),
                first_node.exchange.clone(),
            );
            tx_data.intents.insert(
                first_node.exchange.data.addr.clone(),
                first_node.intent.clone(),
            );
            log_string(format!("tx data: {:?}", tx_data.transfers));
            send_tx(tx_data)
        }
        Err(err) => {
            log_string(format!("Invalid exchange: {}.", err));
        }
    }
}

fn compute_amounts(
    graph: &DiGraph<ExchangeNode, Address>,
    cycle_intents: &[NodeIndex],
) -> Result<HashMap<Exchange, token::Amount>, ResolutionError> {
    let nodes = graph
        .raw_nodes()
        .iter()
        .map(|x| x.weight.exchange.data.clone())
        .collect::<Vec<Exchange>>();
    let mut vars = variables!();

    let mut var_set: HashMap<NodeIndex, VariableDefinition> = HashMap::new();

    let mut intent_graph = graph.filter_map(
        |node_index, node| {
            if cycle_intents.contains(&node_index) {
                let edges = graph.neighbors(node_index);

                *edges
                    .map(|target_node_index| {
                        let target = graph[target_node_index].clone();

                        let variable_definition = variable();
                        var_set.insert(node_index, variable_definition.clone());

                        let var_def = variable_definition
                            .min(target.exchange.data.min_buy)
                            .max(node.exchange.data.max_sell);

                        let var = vars.add(var_def);

                        Some((var, node))
                    })
                    .collect::<Vec<Option<(Variable, &ExchangeNode)>>>()
                    .get(0)
                    .unwrap()
            } else {
                None
            }
        },
        |_edge_index, edge| Some(edge),
    );

    let variables_iter = vars.iter_variables_with_def().map(|(var, _)| var);
    let obj_function: Expression = variables_iter.sum();
    let mut model = vars.maximise(obj_function).using(default_solver);

    let mut constrains = Vec::new();

    // we need to invert the graph otherwise we are not able to build the
    // constrains
    intent_graph.reverse();

    let start = node_index(0);
    depth_first_search(&intent_graph, Some(start), |event| {
        if let DfsEvent::Discover(index, _time) = event {
            let edges = graph.edges(index);

            edges.for_each(|edge| {
                let source = intent_graph[edge.source()];
                let target = intent_graph[edge.target()];

                constrains.push((
                    source.0,
                    target.0,
                    target.1.exchange.data.rate_min.0.to_f64().unwrap(),
                ));
            });
        }
        Control::<()>::Continue
    });

    for constrain in constrains.iter() {
        let constrain = constraint!(constrain.0 >= constrain.1 * constrain.2);
        model = model.with(constrain);
    }

    match model.solve() {
        Ok(solution) => {
            let mut amount_map = HashMap::new();
            let amounts = solution
                .into_inner()
                .iter()
                .map(|(_, amount)| token::Amount::from(*amount))
                .collect::<Vec<_>>();
            nodes.iter().enumerate().for_each(|(index, exchange)| {
                amount_map.insert(exchange.clone(), amounts[index]);
            });
            Ok(amount_map)
        }
        Err(error) => Err(error),
    }
}

// The cycle returned by tarjan_scc only contains the node_index in an arbitrary
// order without edges. we must reorder them to craft the transfer
fn sort_cycle(
    graph: &DiGraph<ExchangeNode, Address>,
    cycle_intents: Vec<NodeIndex>,
) -> Vec<NodeIndex> {
    let mut cycle_ordered = Vec::new();
    let mut cycle_intents = VecDeque::from(cycle_intents);
    let mut to_connect_node = cycle_intents.pop_front().unwrap();
    cycle_ordered.push(to_connect_node);
    while !cycle_intents.is_empty() {
        let pop_node = cycle_intents.pop_front().unwrap();
        if graph.contains_edge(to_connect_node, pop_node) {
            cycle_ordered.push(pop_node);
            to_connect_node = pop_node;
        } else {
            cycle_intents.push_back(pop_node);
        }
    }
    cycle_ordered.reverse();
    cycle_ordered
}

fn find_match_and_send_tx(
    graph: &DiGraph<ExchangeNode, Address>,
) -> Vec<NodeIndex> {
    let mut to_remove_nodes = Vec::new();
    for cycle_intents in petgraph::algo::tarjan_scc(&graph) {
        // a node is a cycle with itself
        if cycle_intents.len() > 1 {
            to_remove_nodes.extend(&cycle_intents);
            create_and_send_tx_data(graph, cycle_intents);
        }
    }
    log_string(format!("found: {:?}", to_remove_nodes));
    to_remove_nodes
}

fn find_match_and_remove_node(graph: &mut DiGraph<ExchangeNode, Address>) {
    let mut to_remove_nodes = find_match_and_send_tx(graph);
    // Must be sorted in reverse order because it removes the node by index
    // otherwise it would not remove the correct node
    to_remove_nodes.sort_by(|a, b| b.cmp(a));
    to_remove_nodes.into_iter().for_each(|i| {
        graph.remove_node(i);
    });
}
