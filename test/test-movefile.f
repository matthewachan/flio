string filename = 'myfile.txt';
create(filename);
string newname = 'renamedfile.txt';

move(filename, newname);

delete(newname);
