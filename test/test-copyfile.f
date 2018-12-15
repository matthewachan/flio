string filename = 'myfile.txt';
string copyname = 'copyfile.txt';
file f = filename;

copy(filename, copyname);

remove(filename);
remove(copyname);
