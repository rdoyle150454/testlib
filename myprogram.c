#include <stdio.h>
#include "FUNC.H"
 
int main()
{
   int first, second, addResult, subtractResult, multiplyResult;
   float divide;
 
   printf("Enter two integers\n");
   scanf("%d%d", &first, &second);
 
   addResult        = add(first, second);
   subtractResult = subtract(first, second);
   multiplyResult = multiply(first, second);
   divide     = first / (float)second;   //typecasting
 
   printf("Sum = %d\n",addResult);
   printf("Difference = %d\n",subtractResult);
   printf("Multiplication = %d\n",multiplyResult);
   printf("Division = %.2f\n",divide);
 
   return 0;
}
