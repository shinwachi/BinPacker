make -k 	 			:  To compile the program	

Main.native GUI 		:  To RUN the GUI MODE, Bin & Pobjs are in random rectangle size.
							user can view all Packing Algorithm by 
								'M' 'm' -> Forward movement
								'N' 'n' -> Backward movement
								'I' 'i' -> Re-Initialization
							
./Main.native Auto		:  Mode for the Automation service to run & extract information in continuity.

./Main.native Hole		:  Same as GUI mode, but Bins will be generated with obstacle of 2x2 block at random position.

./Main.native Tetromino	:  Same as GUI mode, but Pobjs are not rectangle shape but in random Tetromino shape.


Warning1 :  The BestFit is heavy computational algorithm, since all Pobjs and Bins are randomly generated hence their might be
			 situation where processing time of BF varies from 400s to 900s. Please be patience.
			
Warning2 :  Hole mode depicts the Bin Defect scenario in real world. Hence their could be scenario where size of Pobjs is 
			 greater than Bin size. in this case exception will be caused.