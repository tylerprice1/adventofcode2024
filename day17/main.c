
#include <assert.h>
// #include <Metal/Metal.h>
#include <math.h>
#include <pthread.h>
#include <stdbool.h>
#include <stdio.h>

#define THREAD_COUNT 6
#define THREAD_ARG_COUNT 3
#define LENGTH 16

struct Computer
{
  long a;
  long b;
  long c;
};

struct Program
{
  struct Computer computer;
  int instructionPointer;

  const int instructionsLength;
  const int *instructions;

  const int outputLength;
  int *output;
};

void resetProgram(struct Program *program)
{
  program->computer.a = 0l;
  program->computer.b = 0l;
  program->computer.c = 0l;
  program->instructionPointer = 0;
}

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

/*
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
      program->computer.a = program->computer.a / (1 << comboValue(&program->computer, operand));
      program->instructionPointer += 2;
      break;

    case 1:
      program->computer.b = program->computer.b ^ operand;
      program->instructionPointer += 2;
      break;

    case 2:
      program->computer.b = comboValue(&program->computer, operand) % 8;
      program->instructionPointer += 2;
      break;

    case 3:
      if (program->computer.a > 0)
      {
        program->instructionPointer = operand;
      }
      else
      {
        program->instructionPointer += 2;
      }
      break;

    case 4:
      program->computer.b = program->computer.b ^ program->computer.c;
      program->instructionPointer += 2;
      break;

    case 5:
      program->output[i] = comboValue(&program->computer, operand) % 8;
      i++;
      program->instructionPointer += 2;
      break;

    case 6:
      program->computer.b = program->computer.a / (1 << comboValue(&program->computer, operand));
      program->instructionPointer += 2;
      break;

    case 7:
      program->computer.c = program->computer.a / (1 << comboValue(&program->computer, operand));
      program->instructionPointer += 2;
      break;

    default:
      return -1;
    }
  }

  return i;
}
*/

bool outputsSelf(struct Program *program)
{
  struct Computer *computer = &program->computer;
  int i = 0;

  while (program->instructionPointer < program->instructionsLength)
  {
    const int instruction = program->instructions[program->instructionPointer];
    const int operand = program->instructions[program->instructionPointer + 1];

    switch (instruction)
    {
    case 0:
      computer->a = computer->a / (1 << comboValue(&computer, operand));
      program->instructionPointer += 2;
      break;

    case 1:
      computer->b = computer->b ^ operand;
      program->instructionPointer += 2;
      break;

    case 2:
      computer->b = comboValue(&computer, operand) % 8;
      program->instructionPointer += 2;
      break;

    case 3:
      if (computer->a > 0)
      {
        program->instructionPointer = operand;
      }
      else
      {
        program->instructionPointer += 2;
      }
      break;

    case 4:
      computer->b = computer->b ^ computer->c;
      program->instructionPointer += 2;
      break;

    case 5:
      program->output[i] = comboValue(&computer, operand) % 8;
      if (program->output[i] != program->instructions[i])
      {
        return false;
      }
      i++;
      program->instructionPointer += 2;
      break;

    case 6:
      computer->b = computer->a / (1 << comboValue(&computer, operand));
      program->instructionPointer += 2;
      break;

    case 7:
      computer->c = computer->a / (1 << comboValue(&computer, operand));
      program->instructionPointer += 2;
      break;

    default:
      return false;
    }
  }

  return true;
}

long testRange(long args[THREAD_ARG_COUNT])
{
  const int instructions[LENGTH] = {2, 4, 1, 3, 7, 5, 1, 5, 0, 3, 4, 1, 5, 5, 3, 0};
  int output[LENGTH] = {};

  struct Program program = {
    computer : {
      a : 21539243l,
      b : 0l,
      c : 0l,
    },
    instructionPointer : 0,
    instructions : &instructions,
    instructionsLength : LENGTH,
    output : &output,
    outputLength : LENGTH
  };
  const long index = args[0];
  const long start = args[1];
  const long end = args[2];
  const long range = end - start;

  printf("Starting thread %li for range %li - %li\n", index, start, end);

  for (long i = start; i <= end; i += 1l)
  {
    resetProgram(&program);
    program.computer.a = i;

    if ((i - start) % 1000000000l == 0)
    {
      printf("thread %li: %li / %li => %Lf %%\n", index, i - start, range, (double)100.0 * (((double)i - (double)start) / ((double)range)));
    }

    if (!outputsSelf(&program))
    {
      continue;
    }

    printf("Found a: %li\n", i);
    printOutput(&program, program.outputLength);
    return i;
  }

  return -1l;
}

int main()
{
  const long start = (long)pow(8.0, LENGTH - 1);
  const long end = 8 * start - 1;
  const long chunkSize = 1 + (end - start) / (long)THREAD_COUNT;
  pthread_t threads[THREAD_COUNT];

  long thread_args[THREAD_COUNT][THREAD_ARG_COUNT] = {{}, {}, {}};
  int result_code;

  // create all threads one by one
  for (long i = 0; i < (long)THREAD_COUNT; i++)
  {
    long threadStart = start + i * chunkSize;
    long threadEnd = threadStart + chunkSize;
    printf("In main: Creating thread %d.\n", i);
    thread_args[i][0] = i;
    thread_args[i][1] = threadStart;
    thread_args[i][2] = threadEnd;
    result_code = pthread_create(&threads[i], NULL, &testRange, &thread_args[i]);
    assert(!result_code);
  }

  printf("In main: All threads are created.\n");

  // wait for each thread to complete
  for (int i = 0; i < THREAD_COUNT; i++)
  {
    result_code = pthread_join(threads[i], NULL);
    assert(!result_code);
    printf("In main: Thread %d has ended.\n", i);
  }

  printf("Main program has ended.\n");
  return 0;
}
