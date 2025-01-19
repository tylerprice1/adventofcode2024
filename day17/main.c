
#include <pthread.h>
#include <stdbool.h>
#include <math.h>
#include <stdio.h>

#define THREAD_COUNT 4

struct Computer
{
  long a;
  long b;
  long c;
};

struct Program
{
  struct Computer *computer;
  int instructionPointer;

  const int instructionsLength;
  const int *instructions;

  const int outputLength;
  int *output;
};

int comboValue(struct Computer *computer, int opcode)
{
  switch (opcode)
  {
  case 0:
  case 1:
  case 2:
  case 3:
    return opcode;
  case 4:
    return computer->a;
  case 5:
    return computer->b;
  case 6:
    return computer->c;
  default:
    return -1;
  }
}

void printOutput(struct Program *program, int outputLength)
{
  int *output = program->output;

  printf("[");
  for (int i = 0; i < outputLength; i++)
  {
    printf("%d", output[i]);
    if (i < outputLength - 1)
    {
      printf(",");
    }
  }
  printf("]\n");
}

int execute(struct Program *program)
{
  int i = 0;

  while (program->instructionPointer < program->instructionsLength)
  {
    const int instruction = program->instructions[program->instructionPointer];
    const int operand = program->instructions[program->instructionPointer + 1];

    switch (instruction)
    {
    case 0:
      program->computer->a = program->computer->a / (1 << comboValue(program->computer, operand));
      program->instructionPointer += 2;
      break;

    case 1:
      program->computer->b = program->computer->b ^ operand;
      program->instructionPointer += 2;
      break;

    case 2:
      program->computer->b = comboValue(program->computer, operand) % 8;
      program->instructionPointer += 2;
      break;

    case 3:
      if (program->computer->a > 0)
      {
        program->instructionPointer = operand;
      }
      else
      {
        program->instructionPointer += 2;
      }
      break;

    case 4:
      program->computer->b = program->computer->b ^ program->computer->c;
      program->instructionPointer += 2;
      break;

    case 5:
      program->output[i] = comboValue(program->computer, operand) % 8;
      i++;
      program->instructionPointer += 2;
      break;

    case 6:
      program->computer->b = program->computer->a / (1 << comboValue(program->computer, operand));
      program->instructionPointer += 2;
      break;

    case 7:
      program->computer->c = program->computer->a / (1 << comboValue(program->computer, operand));
      program->instructionPointer += 2;
      break;

    default:
      return -1;
    }
  }

  return i;
}

bool outputsSelf(struct Program *program)
{
  int i = 0;

  while (program->instructionPointer < program->instructionsLength)
  {
    const int instruction = program->instructions[program->instructionPointer];
    const int operand = program->instructions[program->instructionPointer + 1];

    switch (instruction)
    {
    case 0:
      program->computer->a = program->computer->a / (1 << comboValue(program->computer, operand));
      program->instructionPointer += 2;
      break;

    case 1:
      program->computer->b = program->computer->b ^ operand;
      program->instructionPointer += 2;
      break;

    case 2:
      program->computer->b = comboValue(program->computer, operand) % 8;
      program->instructionPointer += 2;
      break;

    case 3:
      if (program->computer->a > 0)
      {
        program->instructionPointer = operand;
      }
      else
      {
        program->instructionPointer += 2;
      }
      break;

    case 4:
      program->computer->b = program->computer->b ^ program->computer->c;
      program->instructionPointer += 2;
      break;

    case 5:
      program->output[i] = comboValue(program->computer, operand) % 8;
      if (program->output[i] != program->instructions[i])
      {
        return false;
      }
      i++;
      program->instructionPointer += 2;
      break;

    case 6:
      program->computer->b = program->computer->a / (1 << comboValue(program->computer, operand));
      program->instructionPointer += 2;
      break;

    case 7:
      program->computer->c = program->computer->a / (1 << comboValue(program->computer, operand));
      program->instructionPointer += 2;
      break;

    default:
      return false;
    }
  }

  return true;
}

int main()
{
  const int LENGTH = 16;
  const int instructions[LENGTH] = {2, 4, 1, 3, 7, 5, 1, 5, 0, 3, 4, 1, 5, 5, 3, 0};
  int output[LENGTH] = {};

  struct Computer computer = {
    a : 21539243l,
    b : 0l,
    c : 0l,
  };

  struct Program program = {
    computer : &computer,
    instructionPointer : 0,
    instructions : &instructions,
    instructionsLength : LENGTH,
    output : &output,
    outputLength : LENGTH
  };

  const long start = (long)pow(8.0, LENGTH - 1);
  const long end = 8 * start - 1;
  const long range = end - start;

  for (long i = start; i <= end; i += 1)
  {
    computer.a = i;
    computer.b = 0l;
    computer.c = 0l;
    program.instructionPointer = 0;

    if ((i - start) % 1000000000l == 0)
    {
      printf("%li / %li => %Lf %%\n", i - start, range, (double)100.0 * (((double)i - (double)start) / ((double)range)));
    }

    if (!outputsSelf(&program))
    {
      continue;
    }

    printf("Found a: %li\n", i);
    // printOutput(&program, outputLength);
    break;
  }

  // Part 1

  return 0;
}
