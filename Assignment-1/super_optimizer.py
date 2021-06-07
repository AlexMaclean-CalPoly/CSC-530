from itertools import product
import subprocess

program_size = 2

def main():
    s = "a"
    with open('instruction_space.s', 'r') as instruction_file:
        instruction_space = [i.strip() for i in instruction_file]

    print(instruction_space)

    for program in product(instruction_space, repeat=program_size):

        with open('optimizing.s', 'w') as outfile:
            print(to_assembly(program), file=outfile)

        assemble_result = subprocess.run(['make', 'tester'], capture_output=True)
        if assemble_result.returncode != 0:
            break

        run_result = subprocess.run(['./tester'], capture_output=True)
        if run_result.stdout == b'1\n':
            print(to_assembly(program))


def to_assembly(instructions: list[str]) -> str:
    instructions = '\n'.join(instructions)
    return f"""section .text
global optimizing
optimizing:

mov r8, rdi
{instructions}
ret"""


if __name__ == '__main__':
    main()
