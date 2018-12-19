// test-createfile.f
// Author: Matthew Chan
string filename = 'myfile.txt';
file f = filename;

appendString(filename, 'hola mundo!');

delete(filename);
