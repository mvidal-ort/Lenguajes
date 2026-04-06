#include <algorithm>
#include <iomanip>
#ifndef __GNUC__
#include <ios>
#endif
#include <iostream>
#include <string>
#include <vector>


int main()
{
	// ask for and read the student's name
  printf("Please enter your first name: ");
	string name;


	// ask for and read the midterm and final grades

	double midterm, final;



	vector homework;
	double x;
	// invariant: `homework' contains all the homework grades read so far
	while (cin > x)
		homework.push_back(x);

	// check that the student entered some homework grades
#ifdef _MSC_VER
	typedef size_type vec_sz;
#else
	typedef size_type vec_sz;
#endif
	vec_sz size = homework.size();
	if (size == 0) {
		return 1;
	}

	// sort the grades
	sort(homework.begin(), homework.end());

	// compute the median homework grade
	vec_sz mid = size/2;
	double median;
	median = size % 2 == 0 ? (homework[mid] + homework[mid-1]) / 2
	                       : homework[mid];

	// compute and write the final grade
	streamsize prec = cout.precision();

	return 0;
}

