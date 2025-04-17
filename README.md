# AES Encryption/Decryption Project

This project is a Python implementation of the AES (Advanced Encryption Standard) algorithm for encrypting and decrypting text files. It supports multiple key sizes and handles block alignment for secure and flexible encryption.

# How It's Made
The project was developed using Python in VS Code. It uses Electronic Code Book (ECB) mode to encrypt text files and applies zero padding (0x00) to align data into 32-character hexadecimal blocks. Any lines within text files that surpass 32 characters will only be read up until the 32-char mark.

# Optimization
Originally this project only implemented AES Encryption/Decryption for 128-bit key sizes. It now supports 128-, 192-, and 256-bit AES key sizes to provide flexibility in meeting various security requirements. This allows users to choose a key strength based on their desired balance of performance and security.

# How to Run
Make sure you have Python 3.x installed.  

Run the encryption or decryption script from the terminal:  

Encryption: "python3 AES2.py e keyFile inputFile keySize"  
Program should output the encrypted message for inputFile - "inputFile.enc"  

Decryption: "python3 AES2.py d keyFile inputFile.enc keySize"  
Program should output the decrypted message for inputFile.enc - "inputFile.enc.dec"

inputFile = plaintext file  
keySize = 128 / 192 / 256
