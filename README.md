# DES-encryption-algorithm
This is The DES (Data Encryption Standard) algorithm (devlped by IBM), in the x86 assembly language.
It currently consists of the encryption algorithm only.
The decryption algortithm will be added shortly.

To run this program, you must have the latest versions of DOSBOX and Turbo Debugger installed. Add the files to one directory called TASM. Add the file to a main drive in your computer (e.g C for Windows users).

Open DosBox and type the following:

-mount c: c:\            (if your files are on c disk)
-cd tasm
-tasm /zi base
-tlink /v base.obj
-td base2

This will lead you to the turbo debugger screen. Press view and then CPU.
This leads you to a simulation of the CPU.
Run the program and the encrypted data will be in addresses 128 - 12E (hexa).
Please note - The data that goes into encryption is currently hardcoded in the datasegment part of the code.
Currrently the algorithm work only on 64 bits blocks of data. This will be changed soon.
