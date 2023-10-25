open Eval

(*Test proposer*)
let _t0 = assert( eval_string "print(42)"  = ["42"] )
         
let _t1 = assert( eval_string 
"\
for i in [1, 2, 3]: 
  print(i) 
"  = ["1";"2";"3"] ) 
         
let _t3 = assert( eval_string
"\
def f(n): 
  if n < 2: 
    return n 
  return f(n-1)+f(n-2) 

print(f(10)) 
" = ["55"] ) 
         
let _t4 = assert( eval_string
"\ 
def range(n): 
  if n>0: 
    return range(n-1)+[n-1] 
  return [] 

print(range(10)) 
" = ["[0, 1, 2, 3, 4, 5, 6, 7, 8, 9]"] ) 
let _t5 = assert( eval_string 
"\
def sub(l,from,to): 
  if from<to: 
    return [l[from]]+sub(l,from+1,to) 
  return [] 

def rev(l): 
  if len(l): 
    return rev((sub(l,1,len(l))))+[l[0]] 
  return [] 

def rev2(l): 
  if len(l)<=1: 
    return l 
  mid = len(l)/2 
  return rev((sub(l,mid,len(l))))+rev((sub(l,0,mid))) 

print(rev([1,2,3])) 
print(rev2(rev([1,2,3]))) 
print(rev2(rev((range(10))))) 
"  = ["[3, 2, 1]"; 
      "[1, 2, 3]"; 
      "[0, 1, 2, 3, 4, 5, 6, 7, 8, 9]"])


(*test Suplementaire*)

let _t6 = assert( eval_string "print(range(15))"  = ["[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14]"])
let _t7 = assert( eval_string "print(range(6, 15))"  = ["[6, 7, 8, 9, 10, 11, 12, 13, 14]"])
let _t8 = assert( eval_string "print(range(6, 15, 2))"  = ["[6, 8, 10, 12, 14]"])


let _t9 = assert( eval_string "print([i*i for i in range(10)])" = ["[0, 1, 4, 9, 16, 25, 36, 49, 64, 81]"])
let _t10 = assert( eval_string 
"\
def fibo(n):
  if n < 2 :
    return n
  return fibo(n-1) + fibo(n-2)

print([fibo(i) for i in range(20)])
" = ["[0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, 1597, 2584, 4181]"])

let _t11 = assert( eval_string 
"\
def stringtolist(str):
  return [c for c in str]

print(stringtolist(\"test fonction\"))
" = ["[t, e, s, t,  , f, o, n, c, t, i, o, n]"])


let _t12 = assert( eval_string 
"\
def tri(tab):
   for i in range(len(tab)):
       min = i
       for j in range(i+1, len(tab)):
           if tab[min] > tab[j]:
               min = j
       tmp = tab[i]
       tab[i] = tab[min]
       tab[min] = tmp
   return tab

print(tri([9, 8, 7, 6, 5, 4, 3, 2, 1, 0]))
print(tri([5, 2, 9, 8, 3, 7, 6, 1, 4, 0]))
print(tri([0, 6, 1, 9, 7, 2, 3, 4, 8, 5]))
print(tri([1, 9, 3, 4, 5, 6, 8, 2, 7, 0]))
" = [ "[0, 1, 2, 3, 4, 5, 6, 7, 8, 9]";
      "[0, 1, 2, 3, 4, 5, 6, 7, 8, 9]";
      "[0, 1, 2, 3, 4, 5, 6, 7, 8, 9]";
      "[0, 1, 2, 3, 4, 5, 6, 7, 8, 9]";
    ])
