// ask for a person's name, and greet the person
#include <iostream>
#include <string>

int main()
{
	// ask for the person's name
cout << "Please enter your first name: ";

	// read the name
string name;     // define `name'
cin >> name;     // read into `name'

	// write a greeting
cout << "Hello, " << name  << "!" << endl;
	return 0;
}

