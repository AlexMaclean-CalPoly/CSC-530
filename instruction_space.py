from itertools import product


registers = ['rax', 'r8', 'r9']

unary_instructions = ['neg']
binary_instructions = ['add', 'sub', 'mov']


def make_instruction_space():
    return ([f'{instr} {r}' for instr in unary_instructions for r in registers] +
            [f'{instr} {", ".join(r)}' for instr in binary_instructions for r in product(registers, repeat=2)])


if __name__ == '__main__':
    with open('instruction_space.s', 'w') as outfile:
        for i in make_instruction_space():
            print(i, file=outfile)
