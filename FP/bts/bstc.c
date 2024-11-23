#include <stdio.h>
#include <stdlib.h>
#include "bst.h"

typedef struct _Node{
 int data; //node will store an integer
 Node *right; // right child
 Node *left; // left child
} Node;

Node * addNode(Node * a, int value)
{
    if(a == NULL){
        a =  malloc(sizeof(Node));
        a->data = value;
        }
    else if(value>a->data)
        a->left = addNode(a->left,value);
    else if(value<a->data)
        a->right =addNode(a->right,value);
    return a;
}

Node * removeNode(Node * a, int value){
    Node * temp;
    if(a==NULL)
        return NULL;
    if (value>a->data)
        a->left = removeNode(a->left, value);
    else if(value<a->data)
        a->right = removeNode(a->right, value);
        
         else {
         temp = a;
        // node with only one child or no child
        if (a->left == NULL)
            a = a->right;
        else if (a->right == NULL) {
            a = a->left;
            free(temp);
        }
        else if(a->right && a->left){
            temp = (a->left);
            a->data = temp->data;
            a->left = removeNode(a->left, a->data);
        }
    }
    return a;
}


void displaySubtree(Node * b)
{
    if(b==NULL) // checking if the a is not null
       return;
    else{
        displaySubtree(b->right);// visiting right child
        printf("%d\n", b->data); // printing data at a
        displaySubtree(b->left); // visiting left child
}
}
int numberLeaves(Node * b){
/* Empty(NULL) Tree */
    if(b == NULL)
        return 0;
    /* Check for leaf node */
    if(b->left == NULL && b->right == NULL)
        return 1;
    /* For internal nodes, return the sum of 
    leaf nodes in left and right sub-tree */
    else
    return numberLeaves(b->left) + numberLeaves(b->right);
}



Node * removeSubtree(Node * a, int value)

{
    if(a==NULL)
        return NULL;
    if (value=a->data){
    removeSubtree(a->left,value);
    removeSubtree(a->right,value);
    }
    return a;
    }
    
    
int nodeDepth (Node * R, Node * b){
    if(b->data != R->data)
      return -1;
      int Depth = -1;
    if(R->data == b->data){
        // Otherwise, check if x is
        // present in the left subtree
         Depth = nodeDepth(R->right, b) >= 0;
        // Otherwise, check if x is
        // present in the right subtree
         Depth = nodeDepth(R->left, b) >= 0;
        // Return depth of the node
        return Depth + 1;
}
    return Depth;
}
      
      // if(N->data > R->data){
//       Depth = nodeDepth(R->left, N);
//          Depth ++;

//       }
//       else if (N->data < R->data){
//       Depth = nodeDepth(R->right, N);
//        Depth ++;
//       }

//       return Depth;
//       }
      
      
      
    
      //if(N->data < R->data){
        //R->left->data = nodeDepth(R->left, N);
        //Depth++;
      //}
       //else if (N->data > R->data){
        //R->right->data = nodeDepth(R->right, N);
        //Depth++;
        
    //   int leftDepth = nodeDepth(R->left);
    //   int rightDepth = nodeDepth(R->right);
    //     if (leftDepth >= rightDepth)
    //     return rightDepth + 1;
    //     else
    //         return leftDepth + 1;