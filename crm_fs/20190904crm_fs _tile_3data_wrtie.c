#include <stdio.h>
#include "string.h"
#include "../../src/lib/test_functions.h"
#include "../../src/defines.h"
#include "../../src/hw/bootup.h"
#include "../../src/hw/icrm/icrm.h"
#include "../../src/hw/mm_registers.h"

#define MY_DELAY 0
//#define NUM_OF_CL 128
#define WORDS_PER_CL 4
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

			(*toGlobal(addr+1,1)) = 0xf00f0000; //1237,39
			signal_remote(1,0,0x000ab111); //1237,57 force write through
			pull_signal_local(1,0x111ab000);
			for(i=0; i<NUM_OF_CL; i++)
			{
				for(k=0; k<WORDS_PER_CL; k++) //1
				{
					a = (*toGlobal(addr+k,1)); //read tile 1 data 80200000->B1200000
				}
			}


		}
	}

	if(get_tileid() == 2)
	{
		if(get_coreid() == 0)   /* core 0 */
		{
			volatile unsigned int i,j,k,a;
			(*toGlobal(addr+3,1)) = 0xE00E0000; //1115,61
			signal_remote(1,2,0x222ab111); //1116,01 force write through
		}

	}
	if(get_tileid() == 1)
	{
		if(get_coreid() == 0)   /* core 0 */
		{
			volatile unsigned int i,j,k,a;
			pull_signal_local(0,0x000ab111); //3 1244,49
			pull_signal_local(2,0x222ab111); //3 1123,37 ok
			signal_remote(0,1,0x111ab000);

		}
	}





	/*	############################
	* 	Put test function above here
	* 	############################
	*/

	shutdown_routine();

}

