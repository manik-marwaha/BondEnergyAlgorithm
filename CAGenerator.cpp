/************************************************************************************
SUBJECT:            DISTRIBUTED DATABSE AND DATA MINING
LEVEL  :            POSTGRADUATE
NAME   :            MANIK MARWAHA
UNI ID :            a1797063
ASSIGNMENT NUMBER : 1 
PROGRAM NAME :      BOND ENERGY ALGORITHM 
**************************************************************************************/

#include <iostream>
#include <fstream>
#include <string>
#include <map>
#include <vector>
#include <cmath>


using namespace std;


template <typename T>
void split(vector<T>& st_resultult, const string& s, const string& text)
{
    int s_len = s.length();
	int start = 0;
	int index;
	while ((index = text.find(s, start)) != -1)
	{
		st_resultult.push_back(text.substr(start, index - start));
		start = index + s_len;
	}
	if (start < text.length())
	{
		st_resultult.push_back(text.substr(start, text.length() - start));
	}
}

// taken from the book to find bond
int calculate_bond(int** aa, int a, int b, int order)
{
    int result = 0;
    for (int i = 0; i < order; i++)
    {
        result += aa[i][a] * aa[i][b];
    }
    return result;
}

// this function will be used to find the best position of a clustered affinity matrix column
int argument_best(int* location, int index)
{
    int best_location = 0;
    int best_location_contribution = 1<<31;
    int i =1;
    while(i <= index)
    {
        if (location[i] > best_location_contribution)
        {
            best_location_contribution = location[i];
            best_location = i;
        }
        i++;
    }
    return best_location;
}

// function to transfer the contents from aa matrix to a new camatrix
int** move(int** aa_1c, int* tempMatrix, int order)
{
    int aa_2c [order][order];
    int i=0;
    for (; i < order; i++)
    {
        for (int j = 0; j < order; j++)
        {
            aa_2c[i][j] = aa_1c[i][j];
        }
    }
    int** caMatrix = new int* [order];
    for (int i = 0; i < order; i++)
    {
        caMatrix[i] = new int [order];
        for (int j = 0; j < order; j++)
        {
            caMatrix[i][j] = aa_2c[tempMatrix[i]][j];
        }
    }
    for (int i = 0; i < order; i++)
    {
        for(int j = 0; j < i; j++)
        {
            int tempMatrix3 = caMatrix[i][j];
            caMatrix[i][j] = caMatrix[j][i];
            caMatrix[j][i] = tempMatrix3;
        }
    }
    return caMatrix;
}


// finding the position of attributes 
// For This function help has been taken from online sources
int* pos(int* tempMatrix, const int pos_best, const int index, const int order)
{
    int* result = new int [order];
    if (pos_best == index)
    {
        result = tempMatrix;
        return result;   
    }
    result[pos_best] = tempMatrix[index];
    int i = 0, j = 0, k = 0;
    for (; i < order+1; i++)
    {
        if (i == pos_best){   
            k++;
            continue;
        } 
        else if (i == index){
            result[k] = tempMatrix[j];
            j++;
            continue;
        }
        if (j != index){
            result[k] = tempMatrix[j];
        }
        j++;
        k++;
    }
    return result;
}


// This function will generate the clustered affinity matrix


void generator(int** aa, int** aa_c, int order)
{
    int index = 3;
    int* tempMatrix = new int [order]; //= {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20};    
    for (int i = 0; i < order; i++)
    {
        tempMatrix[i] = i;
    }
    while (index < order - 1)
    {
        int* location = new int[index+1];
        int i = 1;

        // will calculate the contribution REFERENCE : From the book
        while (i <= index)
        {
            int sum1 = calculate_bond(aa, i - 1, index, order);
            int sum2 = calculate_bond (aa, index, i, order);
            int sum3 = calculate_bond (aa, i - 1, i, order);
            int contribution1 = 2 * sum1 + 2 * sum2 - 2 * sum3;
            location[i] = contribution1;

            i++;
        }
        int ss1 = calculate_bond( aa, index - 1, index, order);
        int ss2 = calculate_bond (aa, index, 0, order);
        int ss3 = calculate_bond (aa, index - 1, 0, order);
        int contribution2 = 2 * ss1 + 2 * ss2 - 2 * ss3;
        location[index] = contribution2;
        
        int pos_best = argument_best(location, index); 
        tempMatrix = pos(tempMatrix, pos_best, index, order);
        aa = move(aa_c, tempMatrix, order);
        index++;
    }
    
    // printing the clustered affinity matrix

    int** caMatrix = new int* [order];
    cout << "      ";
    int i = 1;
    while (i < order - 1){
        cout << "A"<< tempMatrix[i] << "     ";
        i++;
    }
    cout << endl;
    int j=1;
    while (j < order - 1){
        caMatrix[j] = aa[tempMatrix[j]];
        cout << "A" << tempMatrix[j] << "    "; 
        for (int k = 1; k < order - 1; k++)
        {
            cout << caMatrix[j][k] << " ";
        }
        cout << endl;
        j++;
    }
}


int main(int argc, char* argv[])
{
    fstream myfile("C:/Users/MANIK MARWAHA/Desktop/aa_2.txt");
    string line;
    vector<string> l;
    int size = 0;
    while (getline(myfile, line))
    {
        split(l, " ", line);
        if (size == 0)
        {
            size = l.size();   
        }
    }
    size += 2;
    int** aa = new int* [size];
    int** aa_c = new int* [size];
     

    // to read the attributes of the affinit matrix
    int i=0;
    while(i<size)
    {
    	int j = 0;
        aa_c[i] = new int [size];
        aa[i] = new int [size];
        while (j<size)
        {
            if (i == 0 || j == 0 || i == size - 1 || j == size - 1)
            {
                aa[i][j] = 0;
                aa_c[i][j] = 0;
            }
            else
            {
                aa[i][j] = stoi(l[(i - 1) * (size - 2) + (j - 1)]); 
                aa_c[i][j] = stoi(l[(i - 1) * (size - 2) + (j - 1)]); 
            }
            j++;
        }
        i++;
    }
    generator(aa, aa_c, size);
    return 0;
}