#include <stdio.h>
#include <stdlib.h>
#include "bst.h"

typedef struct _Node{
 int data; //node will store an integer
 Node *right; // right child
 Node *left; // left child
} Node;

Node * addNode(Node * root, int value)
{
    if(root == NULL){
        root =  malloc(sizeof(Node));
        root->data = value;
        }
    else if(value>root->data)
        root->left = addNode(root->left,value);
    else if(value<root->data)
        root->right =addNode(root->right,value);
    return root;
}

Node * removeNode(Node * root, int value){
    Node * temp;
    if(root==NULL)
        return NULL;
    if (value>root->data)
        root->left = removeNode(root->left, value);
    else if(value<root->data)
        root->right = removeNode(root->right, value);
        
         else {
         temp = root;
        // node with only one child or no child
        if (root->left == NULL)
            root = root->right;
        else if (root->right == NULL) {
            root = root->left;
            free(temp);
        }
        else if(root->right && root->left){
            temp = (root->left);
            root->data = temp->data;
            root->left = removeNode(root->left, root->data);
        }
    }
    return root;
}


void displaySubtree(Node * N)
{
    if(N==NULL) // checking if the root is not null
       return;
    else{
        displaySubtree(N->right);// visiting right child
        printf("%d\n", N->data); // printing data at root
        displaySubtree(N->left); // visiting left child
}
}
int numberLeaves(Node * N){
/* Empty(NULL) Tree */
    if(N == NULL)
        return 0;
    /* Check for leaf node */
    if(N->left == NULL && N->right == NULL)
        return 1;
    /* For internal nodes, return the sum of 
    leaf nodes in left and right sub-tree */
    else
    return numberLeaves(N->left) + numberLeaves(N->right);
}



Node * removeSubtree(Node * root, int value)

{
    if(root==NULL)
        return NULL;
    if (value=root->data){
    removeSubtree(root->left,value);
    removeSubtree(root->right,value);
    }
    return root;
    }
    
    
int nodeDepth (Node * R, Node * N){
    if(N->data == R->data)
      return 0;
    else if (R->left && R->right = NULL)
      return -1;
    else{
        nodeDepth(R->left,N);
        if (R->left!= NULL)
          return nodeDepth(R->left) + 1; 
        else if(R->left == NULL)
          return -1;
        
    }
    else{
        nodeDepth(R->right,N);
        if (R->right!= NULL)
          return nodeDepth(R->right) + 1; 
        else if(R->right == NULL)
          return -1;
    }
    return nodeDepth;
//       int Depth = -1;
//     if(R->data == N->data){
//         // Otherwise, check if x is
//         // present in the left subtree
//          Depth = nodeDepth(R->right, N) >= 0;

//         // Otherwise, check if x is
//         // present in the right subtree
//          Depth = nodeDepth(R->left, N) >= 0;
//         // Return depth of the node
//         return Depth + 1;
// }
//     return Depth;
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