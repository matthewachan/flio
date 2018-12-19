// test-copyfile.f
// Author: Matthew Chan
string filename = 'myfile.txt';
create(filename);
string copyname = 'copyfile.txt';
file f = fopen(filename);

copy(filename, copyname);

delete(filename);
delete(copyname);
