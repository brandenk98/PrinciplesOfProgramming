/*
 **********************************************
 *  CS314 Principles of Programming Languages *
 *  Spring 2020                               *
 **********************************************
 */
#include <stdio.h>
#include <stdlib.h>

__global__ void strongestNeighborScan_gpu(int * src, int * oldDst, int * newDst, int * oldWeight, int * newWeight, int * madeChanges, int distance, int numEdges) {
	/*YOUR CODE HERE*/


/*find the  number of threads & if of the  current  thread*/
	int allThreads = blockDim.x * gridDim.x;
	int tid = blockIdx.x * blockDim.x + threadIdx.x;


	/*increment by tid by also keeping track of allThreads*/
	for(int i = tid; i< numEdges; i += allThreads){
		if((i-distance) >= 0 && (src[i] == src[i-distance])){
			if(oldWeight[i] > oldWeight[i-distance]){
				newWeight[i] = oldWeight[i];
				newDst[i] = oldDst[i];
			}else{
				(*madeChanges) = 1;
				newWeight[i] = oldWeight[i-distance];
				newDst[i] = oldDst[i-distance];
			}
		}else{
			newDst[i] = oldDst[i];
			newWeight[i] = oldWeight[i];
		}
		
	}

}
