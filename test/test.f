// test.f
// Author: Matthew Chan

// FUNCTION TESTS 
def sum (int a, int b) int { 
	return a + b;
}

int my_sum = sum(5, 10);

def get_hello() {
	return 'hello';
}


// FOR LOOP AND CONDITIONAL TESTS
int a;
for (a = 0; a < 10; a = a + 1) {
	get_hello();
	if (a == 5)
		print('Halfway there!');
	else {
		int count = a;
		print('a is equal to ' + count);
	}
	
	if (not even(a)) {
		print('a is odd.');
	}
	elif (not odd(a)) 
		print('a is even.');
}


// TYPE TESTS
file myfile = '/home/myfile.txt';
dir d = '/home';

foreach f in d {
	print(f.path);
	f = myfile;
}

file input = open('input.txt');
file output = open('output.txt');
read(input, 100) |> write(output);

// ARRAY TESTS
int[5] arr = {1, 2, 3, 4, 5};
arr[5] = 10;
arr[4] = arr[2];

int[10] arr2 = arr;

int[2][2] mat;
