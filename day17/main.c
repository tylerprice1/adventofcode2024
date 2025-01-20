#include <assert.h>
#include <math.h>
#include <pthread.h>
#include <stdbool.h>
#include <stdio.h>

#define THREAD_COUNT 1l
#define THREAD_ARG_COUNT 3l
#define LENGTH 16l

struct Program
{
  long a;
  long b;
  long c;
};

const long instructions[LENGTH] = {2l, 4l, 1l, 3l, 7l, 5l, 1l, 5l, 0l, 3l, 4l, 1l, 5l, 5l, 3l, 0l};
struct Program program = {
  a : 21539243l,
  b : 0l,
  c : 0l,
};

long outputsSelf(struct Program *program, long a)
{
  long b = 0;
  long c = 0;
  long instructionPointer = 0;
  long i = 0;
  long out;
  long combo;
  long instruction;
  long operand;

  while (instructionPointer < LENGTH)
  {
    instruction = instructions[instructionPointer];
    operand = instructions[instructionPointer + 1l];

    switch (operand)
    {
    case 0l:
    case 1l:
    case 2l:
    case 3l:
      combo = operand;
      break;
    case 4l:
      combo = a;
      break;
    case 5l:
      combo = b;
      break;
    case 6l:
      combo = c;
      break;
    default:
      assert(false);
    }

    switch (instruction)
    {
    case 0l:
    {
      a = a >> combo;
      break;
    }

    case 1l:
    {
      b = b ^ operand;
      break;
    }

    case 2l:
    {
      b = combo % 8;
      break;
    }

    case 3l:
    {
      if (a > 0)
      {
        instructionPointer = operand;
        continue;
      }
      break;
    }

    case 4l:
    {
      b = b ^ c;
      break;
    }

    case 5l:
    {
      out = combo % 8;
      if (out != instructions[i])
      {
        return i;
      }
      i++;
      break;
    }

    case 6l:
    {
      b = a >> combo;
      break;
    }

    case 7l:
    {
      c = a >> combo;
      break;
    }

    default:
    {
      assert(false);
      return -1;
    }
    }

    instructionPointer += 2;
  }

  return i;
}

long testRange(long args[THREAD_ARG_COUNT])
{
  const long index = args[0];
  const long start = args[1];
  const long end = args[2];
  const long range = end - start;
  long i, j;

  long maxJ = -1, maxResult = -1;

  printf("Starting thread %li for range %li - %li\n", index + 1, start, end);

  for (j = start; j <= end; j += (010000000000l))
  {
    // printf("thread %li: %f %% (%li / %li)\n", index, (double)100.0 * (((double)i - (double)start) / ((double)range)), i - start, range);

    // for (j = i; j < i + 50000000000 - 1; j++)
    // {
    long result = outputsSelf(&program, j);
    // printf("%li -> %li\n", i, result);
    if (result > maxResult)
    {
      maxJ = j;
      maxResult = result;
      fflush(stdout);
    }
    printf("thread %li: 0o%lo -> %li (0d%li)\n", index + 1, maxJ, maxResult, maxJ);
    // if (result > 0)
    // {
    //   printf("%lo -> %li\n", j, result);
    // }

    if (result != LENGTH)
    {
      continue;
    }

    printf("----- FOUND A: %li -----\n", j); // Answer: 216549846240877
    return j;
    // }

    i = j;
  }

  return -1l;
}

int main()
{
  pthread_t threads[THREAD_COUNT];
  const long start = 06052247155l;     // 35184372088832l;
  const long end = 07777777777777777l; // 8l * start - 1l;
  const long chunkSize = 1l + (end - start) / THREAD_COUNT;

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
    assert(!result_code);
  }

  return 0;
}
