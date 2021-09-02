/************************************************************************************
SUBJECT:            DISTRIBUTED DATABSE AND DATA MINING
LEVEL  :            POSTGRADUATE
NAME   :            MANIK MARWAHA
UNI ID :            a1797063
ASSIGNMENT NUMBER : 1 
PROGRAM NAME :      BOND ENERGY ALGORITHM 
**************************************************************************************/

// REFERENCES - To assist with the code to get the attributes, queries and access frequesncy (ACC) of a relation, some help was taken from online sources - github

#include<iostream>
#include <fstream>
#include <vector>
#include <sstream>
#include <cmath>
#include <cstdio>
#include <string>
#include <iomanip>
#include <cstdlib>
#include <ctime>

using namespace std;

// query class (string type) with label and query as parameters 
class query {
public:
    query(string label, string query) {
        this->qLabel = move(label);
        this->q = move(query);
    }
    string q;
    string qLabel;
};

// attribute class with name and labels as the parameters

class attribute {
public:
    attribute(string label, string name) {
        this->aLabel = move(label);
        this->attName = move(name);
    }
    string attName;
    string aLabel;
};


//*********************************************************************************************************************************************//
// THE FOLLOWING FUNCTION WILL GIVE US THE ATTRIBUTES FOR THE RELATION AS SHOWN BELOW
/* Labels ATTRIBUTE_Name
    A1      PNO
    A2      PNAME
    A3      BUDGET
    A4      LOC
  */

vector<attribute> get_att(const string &file) {

    // calling instance of class attribute
    vector<attribute> att;
    ifstream file1(file);
    string line;
    
    while (getline(file1, line)) {
        if(*line.rbegin() == '\r')     {
            line.erase(line.length() - 1);
        }
        istringstream iss(line);
        string aLabel; 
        string att_name;
        getline(iss, aLabel, ' ');
        getline(iss, att_name, ' ');
        att.emplace_back(aLabel, att_name);
    }
    
    att.erase(att.begin());
    return att;
}

//**********************************************************************************************************************//

// THE FOLLOWING FUNCTION WILL GIVE US THE ATTRIBUTES FOR THE RELATION AS SHOWN BELOW
/*
    q1: SELECT BUDGET FROM PROJ WHERE PNO=Value
    q2: SELECT PNAME, BUDGET FROM PROJ  
    q3: SELECT PNAME FROM PROJ WHERE LOC=Value
    q4: SELECT SUM(BUDGET) FROM PROJ WHERE LOC=Value
    */

vector<query> get_queries(const string &q_file) {
    
    vector<query> qquery;
    ifstream file2(q_file);
    string line;
    
    while (getline(file2, line)) {
        if (*line.rbegin() == '\r')     {
            line.erase(line.length() - 1);
        }
        istringstream iss(line);
        string qLabel;
        string qq;
        getline(iss, qLabel, ' ');
        getline(iss, qq);
        qquery.emplace_back(qLabel, qq);
    }
    
    return qquery;
}

//********************************************************************************************************************//

// THE FOLLOWING FUNCTION WILL READ THE ACCESS FREQUENCY 
/*
          S1 S2 S3
        q1 15 20 10
        q2 5  0  0
        q3 25 25 25
        q4 5  0  0
     */
vector<vector<int>> getACC(const string &acc_file, unsigned long num_queries) {

    vector<vector<int>> access;
    ifstream file3(acc_file);
    string line;
    int queries = -1;
    int sites = 0;
    while (getline(file3, line)) {
        if (*line.rbegin() == '\r')     {
            line.erase(line.length() - 1);
        }
        istringstream iss(line);
        string label; string accline;
        getline(iss, label, ' ');
        getline(iss, accline);
        //size_t is an unsigned integer data type 
        //which can assign only 0 and greater than 0 integer values
        size_t curr;
        size_t nxt = -1;
        int site = 0;
        do {
            curr = nxt + 1;
            nxt = accline.find_first_of(' ', curr);
            if (queries < 0) {
                sites++;
                if (nxt == string::npos) {
                    access = vector<vector<int>>(num_queries, vector<int>(sites, 0));
                }
            } else {
                access[queries][site] = stoi(accline.substr(curr, nxt - curr));
                site++;
            }
            if (nxt == string::npos) {
                queries++;
            }
        }
        while (nxt != string::npos);
    }
    return access;
}

//*************************************************************************************************************//

// Reference : Book

// THE FOLLOWING FUNCTION WILL BUILD THE USAGE MATRIX 
/*
            A1 A2 A3 A4
        q1  1  0  1  0
        q2  0  1  1  0
        q3  0  1  0  1
        q4  0  0  1  0
     */

vector<vector<int>> getUse(vector<attribute> att, vector<query> queries) {
    vector<vector<int>> use = vector<vector<int>>(queries.size(), vector<int>(att.size(), 0));

    
    for(int f = 0; f < queries.size();f++){
        for (int k = 0; k < att.size(); k++) {
            (queries[f].q.find(att[k].attName) != std::string::npos &&
                    !isalnum(queries[f].q.at(queries[f].q.find(att[k].attName) - 1))) ? use[f][k] = 1: use[f][k] = 0;
        }
    }
    return use;
}

//************************************************************************************************************//

//finding affinity matrix from Otsuka-Ochiai coefficient 

//Reference : Book

bool condition = false;

vector<vector<int>> AAmatrix(vector<vector<int>> use, vector<vector<int>> ACC, unsigned long attributes, unsigned long num_queries) {
    
    vector<vector<int>> aff = vector<vector<int>>(attributes, vector<int>(attributes, 0));
    
    int c = 1;

    for(int i=0; i < attributes; i++) 
    {
        int j = 0;
        while(j < attributes) 
        { 
            aff[i][j] = 0;

            if (condition){
                int x = 0;
                while (x < num_queries) {
                    if (use[x][i] == 1 && use[x][j] == 1) {
                        int y = 0;
                        while (y < ACC[x].size()) {
                            aff[i][j] += c * ACC[x][y];
                            y++;
                        }
                    }
                    x++;
                }
            }

            else {
                vector<int> total;
                for (int i = 0; i < num_queries; i++)
                {
                    total.push_back(0);
                }
                int k=0;
                
                while(k < num_queries)
                {
                    total[k] = 0;
                    int l = 0;
                    while(l < ACC[k].size()) 
                    { 
                        total[k] += ACC[k][l];
                        l++;
                    }
                   
                    k++;
                }

                double sum = 0;
                double sum1 = 0;
                double sum2 = 0;
                
                for (int m = 0; m < num_queries; m++) 
                { 
                    sum += (double)(use[m][i]) * (double)(total[m]) * (double)(use[m][j]) * (double)(total[m]);
                    sum1 += (double)(use[m][i]) * (double)(total[m]);
                    sum2 += (double)(use[m][j]) * (double)(total[m]);
                }
                aff[i][j] = static_cast<int>(ceil(sum / sqrt(sum1 * sum2)));
                // negative values to be made 0
                if (aff[i][j] < 0) { 
                    aff[i][j] = 0;
                }

                }
            j++;        
        }
    }
    return aff;
    //returning the final affinity matrix which will also be passed as input for CA matrix calculation.
}


//************************************************************************************************************//
int main (int argc, char *argv[]) {
  
  // intializing variable for attributes , queries and acccess frequenfies files 
    string file1;
    string file2;
    string file3;

    file1 = "C:/Users/MANIK MARWAHA/Desktop/att_2.txt";        // attribute file
    file2 = "C:/Users/MANIK MARWAHA/Desktop/query_2.txt";        // query file
    file3 = "C:/Users/MANIK MARWAHA/Desktop/acc_2.txt";        // ACC file

    vector<attribute> att = get_att(file1);
    vector<query> q = get_queries(file2);
    vector<vector<int>> acc = getACC(file3, q.size());
    vector<vector<int>> usage_matrix = getUse(att, q);
    vector<vector<int>> affinity_matrix = AAmatrix(usage_matrix, acc, att.size(), q.size());
   
    //calculating the affinity matrix 

    //printing affinity matrix 
    for(int i=0; i < affinity_matrix.size(); i++){
        for (int j = 0; j < affinity_matrix[i].size(); j++) {
            cout << affinity_matrix[i][j] << ' ';
        }
        cout << endl;
    }
}
//********************************************************************************************************************//
