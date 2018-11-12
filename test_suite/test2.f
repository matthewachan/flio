
// test2.f
// Author: Gideon Cheruiyot
//ARRAYS

int[5] arr = {1, 2, 3, 4, 6, 56, 5};
arr[5] = 10;
arr[4] = arr[2];
int[105] arr2 = arr;
int[2][2] mat;
int a = 8+6;

//INITIALIZING
int count = 8;
string name = 'flio';
string squad = 'eyob gideon matt justin';
dir d = open('c\mydocuments');
file f = open('c\mydocuments\dictionary.txt');
file f2 = open('c\mydocuments\dictionary2.txt');

//BUILT_IN FUNCTIONS

concat(name,squad);
rmdir(d);
delete(f);
appendString(f, name);
appendFile(f,f2);
readLine(f);
print(name);
read(f,14);
dopen(Users/docs);
//OPERATORS
int x = 3;
int y = 4/3;
int q = 6*6+2;
int q = x + name;
if(q != 0){
   print('yaayy');
}elif(q==1){
   print('wrong');
}else{
   return f;
}
