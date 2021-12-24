import z3
import sys

def transpile(program):
    solver = z3.Optimize()

    inp_counter = 0
    inputs = []
    for i in range(14):
        var = z3.BitVec(f"input_{i}", 64)
        solver.add(var > 0)
        solver.add(var < 10)
        inputs += [var]

    model_number = z3.BitVec("model_number", 64)
    expr = inputs[0]
    for inp in inputs[1:]:
        expr = expr * 10 + inp
    solver.add(model_number == expr)

    fresh_counter = 0
    def fresh():
        nonlocal fresh_counter
        var = z3.BitVec(f"fresh_{fresh_counter}", 64)
        fresh_counter += 1
        return var

    registers = {"w": 0, "x": 0, "y": 0, "z": 0}
    def term(t):
        nonlocal registers
        if t.isalpha():
            return registers[t]
        else:
            return int(t)

    for line in program.splitlines():
        words = line.split()
        if words[0] == "inp":
            registers[words[1]] = inputs[inp_counter]
            inp_counter += 1
        elif words[0] == "add":
            lhs = fresh()
            solver.add(lhs == term(words[1]) + term(words[2]))
            registers[words[1]] = lhs
        elif words[0] == "mul":
            lhs = fresh()
            solver.add(lhs == term(words[1]) * term(words[2]))
            registers[words[1]] = lhs
        elif words[0] == "div":
            lhs = fresh()
            solver.add(term(words[2]) != 0)
            solver.add(lhs == term(words[1]) / term(words[2]))
            registers[words[1]] = lhs
        elif words[0] == "mod":
            lhs = fresh()
            solver.add(term(words[2]) != 0)
            solver.add(lhs == term(words[1]) % term(words[2]))
            registers[words[1]] = lhs
        elif words[0] == "eql":
            lhs = fresh()
            solver.add(lhs == z3.If(term(words[1]) == term(words[2]), z3.BitVecVal(1, 64), z3.BitVecVal(0, 64)))
            registers[words[1]] = lhs
        else:
            raise Exception("boooo")

    solver.add(registers["z"] == 0)
    return solver, model_number

input = sys.stdin.read()

solver, model_number = transpile(input)
solver.maximize(model_number)
if solver.check() != z3.sat:
    raise Exception("booooo")
print(solver.model()[model_number])

solver, model_number = transpile(input)
solver.minimize(model_number)
if solver.check() != z3.sat:
    raise Exception("booooooo")
print(solver.model()[model_number])
