#include <stdio.h>
#include "string.h"
#include "../../src/lib/test_functions.h"
#include "../../src/defines.h"
#include "../../src/hw/bootup.h"
#include "../../src/hw/icrm/icrm.h"
#include "../../src/hw/mm_registers.h"

#define MY_DELAY 0
//#define NUM_OF_CL 128
#define WORDS_PER_CL 8
#define NUM_OF_CL 1
//#define WORDS_PER_CL 1
#define TOTAL_WORDS (NUM_OF_CL*WORDS_PER_CL)
#define DIRTY_NUM 8
#define CHECKER_MASK 0xF00FFFFF
#define SEND_SIG_DATA_MASK 0x000AB111
#define RECEIVE_SIG_DATA_MASK 0x111AB000
#define DMA_LOOP 0
#define FFC_TRAFFIC 0
#define DMA_TYPE HWCIC // HWCPY HWCIC HWDMA

volatile unsigned int *addr0 = (volatile unsigned int *) 0x80000000;
volatile unsigned int *addr1 = (volatile unsigned int *) 0x80100000;
volatile unsigned int *addr2 = (volatile unsigned int *) 0x80200000;
volatile unsigned int *addr3 = (volatile unsigned int *) 0x80300000;
volatile unsigned int *addr4 = (volatile unsigned int *) 0x80400000;
volatile unsigned int *addr5 = (volatile unsigned int *) 0x80500000;
volatile unsigned int *addr6 = (volatile unsigned int *) 0x80600000;
volatile unsigned int *addr7 = (volatile unsigned int *) 0x80700000;

/*	############################
* 	Gaith function
* 	############################
*/
volatile unsigned int tile_mask[NTILE] = {1,1,1,1}; // Work in Progress -> Need to enable the for loops

struct iLet dummy_ilet = {(void*)0, (void*)0, 0, 0};

void local_checker(volatile unsigned int *addr, volatile unsigned int tileID)
{
	volatile unsigned int wr_data = (0xffff0000 & CHECKER_MASK) | (tileID << 24) | (tileID << 20);
	volatile unsigned int correct[WORDS_PER_CL] = {0};
	volatile unsigned int wrong[WORDS_PER_CL] = {0};
	volatile unsigned int i,j,k;

	for(i=0,j=0; i<NUM_OF_CL; i++,j+=WORDS_PER_CL)
	{
		for(k=0; k<DIRTY_NUM; k++)
		{
			if(*(addr+j+k) == wr_data + i + k)
				correct[k]++;
			else
				wrong[k]++;
		}
	}
}

void remote_checker(volatile unsigned int *addr, volatile unsigned int data) //addr , checking data value//
{
	volatile unsigned int correct = 0;
	volatile unsigned int wrong = 0;
	volatile unsigned int i;

	for(i=0; i<TOTAL_WORDS; i++)
	{
		if(*toGlobal(addr+i,1) == data + i) //addr , checking data value only check tile 1//
			correct++;
		else
			wrong++;
	}
}

int send_SignalData(volatile unsigned int tileID)
{
	return ((SEND_SIG_DATA_MASK) || (tileID << 28) || (tileID << 24) || (tileID << 20));
}

int receive_SignalData(volatile unsigned int tileID)
{
	return ((RECEIVE_SIG_DATA_MASK) || (tileID << 8) || (tileID << 4) || (tileID));

}
/*	############################
* 	Gaith function
* 	############################
*/
int main()
{
	bootup_routine(0xf);
	    volatile unsigned int *addr = addr2;/* 0x80200000*/
		volatile unsigned int *addr1 = addr5;/* 0x80500000*/

	/*	############################
	* 	Put test function below here
	* 	############################
	*/

	if(get_tileid() == 0)
	{
		if(get_coreid() == 0)   /* core 0 */
		{
			volatile unsigned int i,j,k,a;
			pull_signal_local(1,0x1c0ab000); //1 start
			for(i=0; i<NUM_OF_CL; i++)
			{
				for(k=0; k<WORDS_PER_CL; k++) //
				{
					a = (*toGlobal(addr+k,1)); //read tile 1 data 80200000->B1200000
				}
			}
			signal_remote(1,0,0x0c0ab111); //2 signal
			pull_signal_local(1,0x1c0ab000); //3 start

			for(i=0; i<NUM_OF_CL; i++)
			{
				(*toGlobal(addr+i+1,1)) = 0xf00f00c0 + i;
			}
			signal_remote(1,0,0x0c0ab111); //4 signal

			pull_signal_local(1,0x1c0ab000); //5 start
			for(i=0; i<NUM_OF_CL; i++)
			{
				for(k=0; k<WORDS_PER_CL; k++) //
				{
					a = (*toGlobal(addr+k,1)); //read tile 1 data 80200000->B1200000
				}
			}
			

		}

		if (get_coreid() == 1)  /* core 1 */
		{
			volatile unsigned int a,i,j,k;
			pull_signal_local(1,0x1c0ab000); //1 start
			for(i=0; i<NUM_OF_CL; i++)
			{
				for(k=0; k<WORDS_PER_CL; k++) //1
				{
					a = (*toGlobal(addr+k,1)); //read tile 1 data 80200000->B1200000
				}
			}
			signal_remote(1,0,0x0c1ab111); //2 
			pull_signal_local(1,0x1c0ab000); //3 start

			for(i=0; i<NUM_OF_CL; i++)
			{
				(*toGlobal(addr+i+3,1)) = 0xE00E00c1 + i;
				
			}
			signal_remote(1,0,0x0c1ab111); //4
			


		}

	}

	if(get_tileid() == 1)
	{
		if(get_coreid() == 0)   /* core 0 */
		{
			volatile unsigned int i,j,k,a;
			for(i=0; i<NUM_OF_CL;i++)
			{
				for(k=0; k<WORDS_PER_CL; k++)
				{
					*(addr+i+k) = 0x01100000+i+k; 
				}
			}
			signal_remote(0,1,0x1c0ab000); //1 signal
			// update sharing table
			pull_signal_local(0,0x0c0ab111); //2 
			pull_signal_local(0,0x0c1ab111); //2 start 

			signal_remote(0,1,0x1c0ab000); //3 signal 
			//tile 0 core 0 and core 1 start write data

			pull_signal_local(0,0x0c0ab111); //4 
			pull_signal_local(0,0x0c1ab111); //4 start 

			*(addr+5) = 0xC00C01C0; 
			signal_remote(0,1,0x1c0ab000); //5 signal

		}

	}




	/*	############################
	* 	Put test function above here
	* 	############################
	*/

	shutdown_routine();

}

