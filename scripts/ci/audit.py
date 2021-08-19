import os
import json
import subprocess
import urllib
import urllib.request
from urllib.request import urlopen

GH_TOKEN = os.environ['GITHUB_TOKEN']

GET_ISSUES_URL = 'https://api.github.com/repos/{}/{}/issues'.format('anoma', 'anoma')
CREATE_ISSUE_URL = 'https://api.github.com/repos/{}/{}/issues'.format('anoma', 'anoma')
MODIFY_ISSUE = 'https://api.github.com/repos/{}/{}/issues/{}'.format('anoma', 'anoma', '{}')
HEADERS = {
    'Content-Type': 'application/json', 
    'Accept': 'application/vnd.github.v3+json', 
    'Authorization': 'token {}'.format(GH_TOKEN)
}

ISSUE_TITLE = 'Cargo Audit'
ISSUE_LABEL = 'audit'


# 0 - not exist,2 already exist, else issue number
def check_issue_status(body: str) -> int:
    params = {'labels': ISSUE_LABEL,'state':'open'}
    params_encoded = urllib.parse.urlencode(params)
    req = urllib.request.Request("{}?{}".format(CREATE_ISSUE_URL, params_encoded), headers=HEADERS)

    with urlopen(req) as response:
        issues = json.load(response)
        for issue in issues:
            title_check = issue['title'] == ISSUE_TITLE
            label_check = ISSUE_LABEL in list(map(lambda label: label['name'], issue['labels']))
            if title_check and label_check:
                if issue['body'] == body:
                    return 2
                return issue['number']
        return 0


def modify_issue(issue_number: int, body: str):
    body = {"body": body}
    encoded_body = json.dumps(body).encode('ascii')
    req = urllib.request.Request(MODIFY_ISSUE.format(issue_number), data=encoded_body, headers=HEADERS)
    req.get_method = lambda: 'PATCH'

    with urlopen(req) as response:
        json.load(response)


def create_issue(body: str):
    body = {"title": ISSUE_TITLE, "body": body, "labels": [ISSUE_LABEL]}
    encoded_body = json.dumps(body).encode('ascii')
    req = urllib.request.Request(CREATE_ISSUE_URL, data=encoded_body, headers=HEADERS)

    with urlopen(req) as response:
        json.load(response)


issue_template = '# Vulnerabilities \n{}'
table_header = '| Id  | Package  | Title  | Date  |\n|----:|---------:|-------:|------:|'
table_row = '|[{0}]({advisory_db}{0})|{1}|{2}|{3}|'

table = [table_header]

command = ['cargo', 'audit', '--json']
p = subprocess.Popen(command, stdout=subprocess.PIPE)
output = p.stdout.read()
retcode = p.wait()

vulnerabilities = json.loads(output)['vulnerabilities']
if int(vulnerabilities['count']) == 0:
    print("No vulnerabilities found.")
    exit(0)

for vulnerability in vulnerabilities['list']:
    vuln_description = vulnerability['advisory']
    vuln_id = vuln_description['id']
    vuln_title = vuln_description['title']
    vuln_package = vuln_description['package']
    vuln_date = vuln_description['date']
    new_table_row = table_row.format(vuln_id, vuln_package, vuln_title, vuln_date, 
        # link issues by their ID to the advisory DB
        advisory_db = 'https://rustsec.org/advisories/')
    table.append(new_table_row)

table_rendered = '\n'.join(table)
issue_body = issue_template.format(table_rendered)

issue_status = check_issue_status(issue_body)

if issue_status == 0:
    print("Create new issue.")
    create_issue(issue_body)
elif issue_status == 2:
    print("Issue already created.")
else:
    print("Issue updated.")
    modify_issue(issue_status, issue_body)
