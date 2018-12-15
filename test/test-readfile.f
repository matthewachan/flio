string filename = 'myfile.txt';
file f = filename;

appendString(filename, 'hola mundo!');
prints(readLine(f));
remove(filename);
