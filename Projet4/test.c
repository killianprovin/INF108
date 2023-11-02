int fib(int n) {
    if(n<=1) return n;
    return fib(n-1)+fib(n-2);
}

int syr(int n) {
    print_int(n);
    if(n==1) return 0;
    if(n%2) return syr(3*n+1);
    return syr(n/2);
}

int fact(int n) {
    if(n<=1) return 1;
    return fact(n-1)*n;
}
int mod(int a, int b){
    print_int(a%b);
}

int main(){
    int a = 100;
    int b = 7;
    print_int(fib(b));
    print_int(a);
    mod(a, b);
}