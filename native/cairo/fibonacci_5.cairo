func main() {
    // Call fib(1, 1, 5).
    let result: felt = fib(1, 1, 5);

    // Make sure the 5th Fibonacci number is 13.
    assert result = 13;
    ret;
}

func fib(first_element, second_element, n) -> (res: felt) {
    jmp fib_body if n != 0;
    tempvar result = second_element;
    return (second_element,);

    fib_body:
    tempvar y = first_element + second_element;
    return fib(second_element, y, n - 1);
}
