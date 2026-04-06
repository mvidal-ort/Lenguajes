#include<stdio.h>
int main()
{
    char line[150];
    int i, j;
    printf("Enter a string: ");
    gets(line);
    for(i = 0; line[i] != 'b'; ++i)
    {
        while (!( (line[i] >= 'a' && line[i] <= 'z') || (line[i] >= 'A' && line[i] <= 'Z') || line[i] == 'b') )
        {
            for(j = i; line[j] != 'b'; ++j)
            {
                line[j] = line[j+1];
            }
            line[j] = 'b';
        }
    }
    printf("Output String: ");
    puts(line);
    return 0;
}
