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
			pull_signal_local(0,0x00cab1c0);
			(*toGlobal(addr+1,1)) = 0xf00fF00F; //1234,15
			signal_remote(1,0,0x000ab0C0);
		}
		if(get_coreid() == 1)   /* core 1 */
		{
			volatile unsigned int i,j,k,a;
			pull_signal_local(0,0x00cab1c0);
			(*toGlobal(addr+3,1)) = 0xE00EE00E; //1215,23
			signal_remote(1,1,0x000ab0C1);
		}
		if(get_coreid() == 2)   /* core 2 */
		{
			volatile unsigned int i,j,k,a;
			pull_signal_local(0,0x000ab1c0); //read from tile 1
			for(i=0; i<NUM_OF_CL; i++)
			{
				for(k=0; k<WORDS_PER_CL; k++) //
				{
					a = (*toGlobal(addr+k,1)); //read tile 1 data 80200000->B1200000
				}
			}
			signal_remote(1,2,0x111ab0c2);
		}

	}


	if(get_tileid() == 1)
	{
		if(get_coreid() == 0)   /* core 0 */
		{
			volatile unsigned int i,j,k,a; //
			for(i=0; i<NUM_OF_CL;i++)
			{
				for(k=0; k<WORDS_PER_CL; k++)
				{
					*(addr+i+k) = 0x10011001+i+k;
				}
			}
			signal_remote(0,0,0x000ab1c0);
			pull_signal_local(2,0x111ab0c2);
			signal_remote(0,0,0x00cab1c0); //trigger tile 0 core 0 and core 1 start writeing

			*(addr+2) = 0x20022002;

			pull_signal_local(0,0x000ab0C0);
			pull_signal_local(1,0x000ab0C1);
			signal_remote(2,0,0x222ab1C0); // reading final data signal to tile 2
		}
	}

	if(get_tileid() == 2)
	{
		if(get_coreid() == 0)   /* core 0 */
		{
			volatile unsigned int i,j,k,a;
			pull_signal_local(0,0x222ab1C0);
			for(i=0; i<NUM_OF_CL; i++)
			{
				for(k=0; k<WORDS_PER_CL; k++)
				{
					a = (*toGlobal(addr+k,1));
				}
			}

		}

	}



	/*	############################
	* 	Put test function above here
	* 	############################
	*/

	shutdown_routine();

}

