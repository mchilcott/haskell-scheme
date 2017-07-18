from testbench import tester

t = tester()

t.config = {
    "cmd" : ["./Parser"],
    "name" : "Scheme Parser",
    "log_level" : tester.LOG_ERR
}


def run(t):
    # Atom Tests
    test = {
        "name": "Parser::Atom",
        "args": ["atom"],
        "input": "",
        "stdout_check" : t.outputContains("Atom \"atom\"")
        }

    t.run_test(test)


    # Boolean Tests
    test['name'] = "Parser::BoolTrue"
    test['args'] = ["#t"]
    test['stdout_check'] = t.outputContains("Bool True")
    t.run_test(test)

    test['name'] = "Parser::BoolFalse"
    test['args'] = ["#f"]
    test['stdout_check'] = t.outputContains("Bool False")
    t.run_test(test)

    # Character
    test['name'] = "Parser::Char:a"
    test['args'] = ["#\\a"]
    test['stdout_check'] = t.outputContains("Character 'a'")
    t.run_test(test)

    test['name'] = "Parser::Char:A"
    test['args'] = ["#\\A"]
    test['stdout_check'] = t.outputContains("Character 'A'")
    t.run_test(test)

    test['name'] = "Parser::Char:("
    test['args'] = ["#\\("]
    test['stdout_check'] = t.outputContains("Character '('")
    t.run_test(test)

    test['name'] = "Parser::Char:space"
    test['args'] = ["#\\ "]
    test['stdout_check'] = t.outputContains("Character ' '")
    t.run_test(test)

    test['name'] = "Parser::Char:space_long"
    test['args'] = ["#\\space"]
    test['stdout_check'] = t.outputContains("Character ' '")
    t.run_test(test)

    test['name'] = "Parser::Char:newline"
    test['args'] = ["#\\newline"]
    test['stdout_check'] = t.outputContains("Character '\\n'")
    t.run_test(test)

    # Numbers
    test['name'] = "Parser::Number"
    test['args'] = ["8375"]
    test['stdout_check'] = t.outputContains("Number 8375")
    t.run_test(test)
    
    test['name'] = "Parser::Number:Hex"
    test['args'] = ["#xDEAD"]
    test['stdout_check'] = t.outputContains("Number ")
    t.run_test(test)
    
    
    test['name'] = "Parser::Number:Binary"
    test['args'] = ["#b1001"]
    test['stdout_check'] = t.outputContains("Number 9")
    t.run_test(test)
    
    
    test['name'] = "Parser::Number:Octal"
    test['args'] = ["#o10"]
    test['stdout_check'] = t.outputContains("Number 8")
    t.run_test(test)
    
    
    test['name'] = "Parser::Number:Dec"
    test['args'] = ["#d87233"]
    test['stdout_check'] = t.outputContains("Number 87233")
    t.run_test(test)
    

    # String
    test['name'] = "Parser::String"
    test['args'] = ["\"string\""]
    test['stdout_check'] = t.outputContains("String \"string\"")
    t.run_test(test)

    test['name'] = "Parser::String::BackslashQuote"
    test['args'] = ["\"\\\"\""]
    test['stdout_check'] = t.outputContains("String \"\\\"\"")
    t.run_test(test)
    
    test['name'] = "Parser::String::BackslashOther"
    test['args'] = ["\"\\f\""]
    test['stdout_check'] = t.outputContains("String \"\\\\f\"")
    t.run_test(test)


    # List
    test['name'] = "Parser::List"
    test['args'] = ["(atom 987 02)"]
    test['stdout_check'] = t.outputContains("List [Atom \"atom\",Number 987,Number 2]")
    t.run_test(test)

    test['name'] = "Parser::List:Nested"
    test['args'] = ["(a (nested) test)"]
    test['stdout_check'] = t.outputContains("List [Atom \"a\",List [Atom \"nested\"],Atom \"test\"]")
    t.run_test(test)

    test['name'] = "Parser::List:Dotted"
    test['args'] = ["(dotted . list)"]
    test['stdout_check'] = t.outputContains("DottedList [Atom \"dotted\"] (Atom \"list\")")
    t.run_test(test)

    test['name'] = "Parser::List:Quoted"
    test['args'] = ["(atom 'test)"]
    test['stdout_check'] = t.outputContains("List [Atom \"atom\",List [Atom \"quote\",Atom \"test\"]]")
    t.run_test(test)

    test['name'] = "Parser::List:Quasiquoted"
    test['args'] = ["(atom `test)"]
    test['stdout_check'] = t.outputContains("List [Atom \"atom\",List [Atom \"quasiquote\",Atom \"test\"]]")
    t.run_test(test)

    test['name'] = "Parser::List:Unuoted"
    test['args'] = ["(atom ,test)"]
    test['stdout_check'] = t.outputContains("List [Atom \"atom\",List [Atom \"unquote\",Atom \"test\"]]")
    t.run_test(test)

    
run(t)

t.final_print()
