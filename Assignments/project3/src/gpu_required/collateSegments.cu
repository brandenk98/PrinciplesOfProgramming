/*
 **********************************************
 *  CS314 Principles of Programming Languages *
 *  Spring 2020                               *
 **********************************************
 */
#include <stdio.h>
#include <stdlib.h>

__global__ void collateSegments_gpu(int * src, int * scanResult, int * output, int numEdges) {
	/*YOUR CODE HERE*/

	int allThreads = blockDim.x * gridDim.x;
	int tid = blockIdx.x * blockDim.x + threadIdx.x;

	for(int i = tid; i < numEdges; i += allThreads){
		if (src[i] != src[i+1]){
			output[src[i]] = scanResult[i];
		}
	}
	
	if(tid >= numEdges && allThreads !=0){
		return;
	}
}
