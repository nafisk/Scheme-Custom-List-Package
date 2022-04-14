//Deepankar Chakraborty
//We can probably use some ideas from this algo. to use in the sort. We can probably take advantage of inser-at function. 


/*Selection Sort*/ 
//#1) Assume left side is sorted
//#2) Compare left value with all of the right one. 
//#3) Go through all right to find the smallest one compare to i'th index
//#4) Then swap the two.
//https://youtu.be/g-PGLbMth_g 

#include<iostream>
#include<vector>
#include<algorithm>
 
using namespace std; 
void selection_sort(vector<int>& list, int& counter){
    int min; 

    for (int i = 0; i < list.size()-1; i++){
        min = i; 
        for (int j = i+1; j < list.size(); j++){
            if(list[j] < list[min]) 
            min = j;  
            counter++; 
        }
        swap(list[min],list[i]);
    }
    
}
int main () {

    vector<int> list ={61,33,8,1,5,39,42,34,24,68,123,7,96,13,55,3,2,73}; 
    int counter=0; 
    selection_sort(list, counter); 

    for (auto &&i : list)
    {
        cout<<i<<" "; 
    }
    cout<< "\n" << counter<<endl;
    return 0; 
}
