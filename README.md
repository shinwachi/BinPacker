

CS51 final project to demonstrate bin packing algorithm using OCaml.  [Watch the video here](https://vimeo.com/93603629).

Authors: Zeeshan Qamar and Shin Wachi

### Compile the packer
```
make -k         : To compile the program	
```

### Run the packer
```
Main.native GUI : To RUN the GUI MODE, Bin & Pobjs are in random rectangle size.
                  user can view all Packing Algorithm by 
                  'M' 'm' -> Forward movement
                  'N' 'n' -> Backward movement
                  'I' 'i' -> Re-Initialization
			
./Main.native Auto : Mode for the Automation service to run & extract 
                     information in continuity.

./Main.native Hole : Same as GUI mode, but Bins will be generated with obstacle 
                     of 2x2 block at random position.

./Main.native Tetromino	: Same as GUI mode, but Pobjs are not rectangle shape 
                          but in random Tetromino shape.

Warning1 : The BestFit is computationally heavy algorithm, since all Pobjs and 
           Bins are randomly generated hence there might be in situation where 
           processing time of BF varies from 400s to 900s. Please be patient.

Warning2 : Hole mode depicts the Bin Defect scenario in real world. Hence there
           could be scenario where size of Pobjs is greater than Bin size. 
           In this case exception will happen (this is an extra feature).
```
