// ask for a person's name, and greet the person
#include <stdio.h>

int main()
{
  // ask for the person's name
  printf("Please enter your first name: \n");

  // read the name
  char name[20];
  scanf("%s", name);     // read into `name'

  // write a greeting
  printf("Hello, %s!\n",name);
  return 0;
}

