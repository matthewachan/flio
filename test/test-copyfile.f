string filename = 'myfile.txt';
string copyname = 'copyfile.txt';
file f = filename;

copy(filename, copyname);

delete(filename);
delete(copyname);
