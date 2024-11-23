#include <stdio.h>
#include <unistd.h>
#include <pthread.h>

void do_one_thing();
void do_another_thing();

int main(void)
{
  pthread_t thread1,  thread2;

  pthread_create(&thread1,
          NULL,
          (void *) do_one_thing,
          NULL);

  pthread_create(&thread2,
          NULL,
          (void *) do_another_thing,
          NULL);
	/* Try different options */
  //pthread_join(thread1,  NULL);
  //pthread_join(thread2,  NULL);
  //sleep(1);
  //return 0;
  //pthread_exit(0);
}

void do_one_thing()
{
  int i, j, x;

  for (i = 0; i  <  200;  i++) {
    printf("doing one thing\n");
  }
  	
}

void do_another_thing()
{
  int i, j, x;

  for (i = 0;    i  <  200;  i++) {
    printf("doing another \n");
  }
}

