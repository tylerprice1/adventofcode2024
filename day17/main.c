#include <assert.h>
#include <math.h>
#include <pthread.h>
#include <stdbool.h>
#include <stdio.h>

#define THREAD_COUNT 8
#define THREAD_ARG_COUNT 3
#define LENGTH 16

struct Program
{
  long a;
  long b;
  long c;
};

const int instructions[LENGTH] = {2, 4, 1, 3, 7, 5, 1, 5, 0, 3, 4, 1, 5, 5, 3, 0};
struct Program program = {
  a : 21539243l,
  b : 0l,
  c : 0l,
};

bool outputsSelf(struct Program *program, long a)
{
  long b = 0;
  long c = 0;
  int instructionPointer = 0;
  int i = 0;
  int out;
  int combo;
  int instruction;
  int operand;

  while (instructionPointer < LENGTH)
  {
    instruction = instructions[instructionPointer];
    operand = instructions[instructionPointer + 1];

    switch (operand)
    {
    case 0:
    case 1:
    case 2:
    case 3:
      combo = operand;
    case 4:
      combo = a;
    case 5:
      combo = b;
    case 6:
      combo = c;
    default:
      combo = -1;
    }

    switch (instruction)
    {
    case 0:
      a = a >> combo;
      break;

    case 1:
      b = b ^ operand;
      break;

    case 2:
      b = combo % 8;
      break;

    case 3:
      if (a > 0)
      {
        instructionPointer = operand;
        continue;
      }
      break;

    case 4:
      b = b ^ c;
      break;

    case 5:
      out = combo % 8;
      if (out != instructions[i])
      {
        return false;
      }
      i++;
      break;

    case 6:
      b = a >> combo;
      break;

    case 7:
      c = a >> combo;
      break;

    default:
      return false;
    }

    instructionPointer += 2;
  }

  return true;
}

long testRange(long args[THREAD_ARG_COUNT])
{
  const long index = args[0];
  const long start = args[1];
  const long end = args[2];
  const long range = end - start;
  long i, j;

  printf("Starting thread %li for range %li - %li\n", index + 1, start, end);

  for (i = start + 985000000000l; i <= end; i++)
  {
    printf("thread %li: %Lf %% (%li / %li)\n", index, (double)100.0 * (((double)i - (double)start) / ((double)range)), i - start, range);

    for (j = i; j < i + 50000000000 - 1; j++)
    {
      if (!outputsSelf(&program, j))
      {
        continue;
      }

      printf("----- FOUND A: %li -----\n", j);
      return j;
    }

    i = j;
  }

  return -1l;
}

int main()
{
  pthread_t threads[THREAD_COUNT];
  const long start = (long)pow(8.0, LENGTH - 1);
  const long end = 8 * start - 1;
  const long chunkSize = 1 + (end - start) / (long)THREAD_COUNT;

  long thread_args[THREAD_COUNT][THREAD_ARG_COUNT] = {{}, {}, {}};
  int result_code;

  // create all threads one by one
  for (long i = 0; i < (long)THREAD_COUNT; i++)
  {
    long threadStart = start + i * chunkSize;
    long threadEnd = threadStart + chunkSize;
    thread_args[i][0] = i;
    thread_args[i][1] = threadStart;
    thread_args[i][2] = threadEnd;
    result_code = pthread_create(&threads[i], NULL, &testRange, &thread_args[i]);
    assert(!result_code);
  }

  // wait for each thread to complete
  for (int i = 0; i < THREAD_COUNT; i++)
  {
    result_code = pthread_join(threads[i], NULL);
    printf("In main: Thread %d has ended.\n", i);
    assert(!result_code);
  }

  printf("Main program has ended.\n");
  return 0;
}
