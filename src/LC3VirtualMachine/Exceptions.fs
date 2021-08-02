namespace LC3VirtualMachine

exception EvaluatorException of string

module EvaluatorExceptions =
    let badOpCodeException (arg: string): exn = EvaluatorException("Bad op code '" + arg + "'")
    
    let badTrapCodeException (arg: string): exn = EvaluatorException("Bad trap code '" + arg + "'")

    let unknownOpCodeException (arg: string): exn = EvaluatorException("Unknown op code '" + arg + "'")

    let unknownTrapCodeException (arg: string): exn = EvaluatorException("Unknown trap code '" + arg + "'")