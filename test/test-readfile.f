// test-readfile.f
// Author: Matthew Chan
string filename = 'myfile.txt';
create(filename);
file f = fopen(filename);

appendString(filename, 'hola mundo!');
prints(readLine(f));
delete(filename);
