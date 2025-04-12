import sys

# key expansion functions ------------------------------------

# create a matrix from list of 4 words, each a list of 4 bytes (is being iterated 11 times by keyexpansion)
def roundKeyMatrix(wordList, roundInd):
    #takes the round to find the 4 corresponding 4 byte words
    firstInd = roundInd * 4
    lastInd = firstInd + 4
    # list of 4 words, each 4 bytes
    roundList = wordList[firstInd : lastInd]
    #make matrix with 4 words -- swaps the col rows 
    matrix = [[roundList[col][row] for col in range(4)] for row in range(4)]
    return matrix

# s box to 4 byte input word to each byte
def subWord(byteWord):
    sBox = [
        [0x63, 0x7c, 0x77, 0x7b, 0xf2, 0x6b, 0x6f, 0xc5, 0x30, 0x01, 0x67, 0x2b, 0xfe, 0xd7, 0xab, 0x76],
        [0xca, 0x82, 0xc9, 0x7d, 0xfa, 0x59, 0x47, 0xf0, 0xad, 0xd4, 0xa2, 0xaf, 0x9c, 0xa4, 0x72, 0xc0],
        [0xb7, 0xfd, 0x93, 0x26, 0x36, 0x3f, 0xf7, 0xcc, 0x34, 0xa5, 0xe5, 0xf1, 0x71, 0xd8, 0x31, 0x15],
        [0x04, 0xc7, 0x23, 0xc3, 0x18, 0x96, 0x05, 0x9a, 0x07, 0x12, 0x80, 0xe2, 0xeb, 0x27, 0xb2, 0x75],
        [0x09, 0x83, 0x2c, 0x1a, 0x1b, 0x6e, 0x5a, 0xa0, 0x52, 0x3b, 0xd6, 0xb3, 0x29, 0xe3, 0x2f, 0x84],
        [0x53, 0xd1, 0x00, 0xed, 0x20, 0xfc, 0xb1, 0x5b, 0x6a, 0xcb, 0xbe, 0x39, 0x4a, 0x4c, 0x58, 0xcf],
        [0xd0, 0xef, 0xaa, 0xfb, 0x43, 0x4d, 0x33, 0x85, 0x45, 0xf9, 0x02, 0x7f, 0x50, 0x3c, 0x9f, 0xa8],
        [0x51, 0xa3, 0x40, 0x8f, 0x92, 0x9d, 0x38, 0xf5, 0xbc, 0xb6, 0xda, 0x21, 0x10, 0xff, 0xf3, 0xd2],
        [0xcd, 0x0c, 0x13, 0xec, 0x5f, 0x97, 0x44, 0x17, 0xc4, 0xa7, 0x7e, 0x3d, 0x64, 0x5d, 0x19, 0x73],
        [0x60, 0x81, 0x4f, 0xdc, 0x22, 0x2a, 0x90, 0x88, 0x46, 0xee, 0xb8, 0x14, 0xde, 0x5e, 0x0b, 0xdb],
        [0xe0, 0x32, 0x3a, 0x0a, 0x49, 0x06, 0x24, 0x5c, 0xc2, 0xd3, 0xac, 0x62, 0x91, 0x95, 0xe4, 0x79],
        [0xe7, 0xc8, 0x37, 0x6d, 0x8d, 0xd5, 0x4e, 0xa9, 0x6c, 0x56, 0xf4, 0xea, 0x65, 0x7a, 0xae, 0x08],
        [0xba, 0x78, 0x25, 0x2e, 0x1c, 0xa6, 0xb4, 0xc6, 0xe8, 0xdd, 0x74, 0x1f, 0x4b, 0xbd, 0x8b, 0x8a],
        [0x70, 0x3e, 0xb5, 0x66, 0x48, 0x03, 0xf6, 0x0e, 0x61, 0x35, 0x57, 0xb9, 0x86, 0xc1, 0x1d, 0x9e],
        [0xe1, 0xf8, 0x98, 0x11, 0x69, 0xd9, 0x8e, 0x94, 0x9b, 0x1e, 0x87, 0xe9, 0xce, 0x55, 0x28, 0xdf],
        [0x8c, 0xa1, 0x89, 0x0d, 0xbf, 0xe6, 0x42, 0x68, 0x41, 0x99, 0x2d, 0x0f, 0xb0, 0x54, 0xbb, 0x16]
    ]
    #iterate through each byte and replace
    for byte in range(4):
        sBoxRow = (byteWord[byte] >>4) & 0x0F #must shift right to get just the value of the upper nibble
        sBoxCol = byteWord[byte] & 0x0F
        #substitute based on look up of each nibble of the value + the corresponding value in s-box
        byteWord[byte] = sBox[sBoxRow][sBoxCol]
    return byteWord

# rotate position 1230
def rotWord(byteWord):
    # slice 1st index from the rest + adds back 1st index -- rotate left one
    return byteWord[1:] + byteWord[:1]
    # temp = byteWord[:]
    # byteWord[0] = temp[1]
    # byteWord[1] = temp[2]
    # byteWord[2] = temp[3]
    # byteWord[3] = temp[0]
    # return byteWord

#128 - 4 word key, 16 bytes, 10 rounds
#192 - 6 word key, 24 bytes, 12 rounds
#256 - 8 word key, 32 bytes, 14 rounds
#11 rounds for 128--  need 4 words of key data -- 44 words
def keyExpansion(k, mode): 
    #rcon[i] round constant word array
    rCon = [
        0x00, 0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40, 0x80,
        0x1B, 0x36, 0x6C, 0xD8, 0xAB
    ]
    #mode 128
    wKey = 4
    wRounds = 10
    if mode == "192":
        wKey = 6
        wRounds = 12
    elif mode == "256":
        wKey = 8
        wRounds = 14
    # linear array of 4 byte words [wList_i] 0 <= i < 4*(rounds+1) --44 for 128
    wList = []
    i = 0  #initialize index for words to put in wList
    while (i < wKey):  #for the first 4 words
        #take the 4 bytes and form 32 bit word (list of 4 bytes)
        word = [k[4*i], k[4*i+1], k[4*i+2], k[4*i+3]]
        wList.append(word) #add 4 byte word to list -- 44 words total eventually for 128
        i += 1     
    for i in range (wKey, 4*(wRounds+1)): #44 words total for 128
        tempWord = wList[i-1][:] #copy previous word of 4 bytes into tempWord
        if (i % wKey == 0): #if i is multiple of nk
            tempWord = rotWord(tempWord) #transformation = rotword followed by subword
            tempWord = subWord(tempWord)
            #round constant -- divide i by wKey to get round number
            #rcon is a 4byte word where the last 3 bytes are 0 -- xor only affects 1st byte
            # / creates float; // is int divison
            tempWord[0] ^= rCon[i//wKey]

        elif ((wKey > 6) & (i & wKey == 4)): #this is for aes 256
            tempWord = subWord(tempWord)

        #every following wList[i] after wKeyth word --wList[i] = wList[i-1] ^ wList[i-wKey] 
        # zip makes list of pairs of the two list elements -- creates tuples
        word_i = [x ^ y for x, y in zip(wList[i-wKey], tempWord)] #this should create new list with each byte pair in wList[i-wkey] and temp XORed
        wList.append(word_i) #add into word list
    
    rkMtxList = []
    #iterate through rounds+1 times
    for i in range(wRounds+1):
        rkMtxList.append(roundKeyMatrix(wList, i)) #make a list of rounds+1 matrices by finding the matrix for each round
    return rkMtxList


#inverse decryption functions--------------------

def invMixColumns(state):
    #matrix
    #each column = column x fixed 4x4 matrix of ints
    # copies each row per row in state -- copies each row list into a larger list to recreate matrix
    tempState = [row[:] for row in state] 
    # iterate through columns
    for i in range(4): #change each value down the column using GF multiplication
        tempState[0][i] = gfMult(0x0e, state[0][i]) ^ gfMult(0x0b, state[1][i]) ^ gfMult(0x0d, state[2][i]) ^ gfMult(0x09, state[3][i]) # 14 11 13 9
        tempState[1][i] = gfMult(0x09, state[0][i]) ^ gfMult(0x0e, state[1][i]) ^ gfMult(0x0b, state[2][i]) ^ gfMult(0x0d, state[3][i]) # 9 14 11 13
        tempState[2][i] = gfMult(0x0d, state[0][i]) ^ gfMult(0x09, state[1][i]) ^ gfMult(0x0e, state[2][i]) ^ gfMult(0x0b, state[3][i]) # 13 9 14 11
        tempState[3][i] = gfMult(0x0b, state[0][i]) ^ gfMult(0x0d, state[1][i]) ^ gfMult(0x09, state[2][i]) ^ gfMult(0x0e, state[3][i]) # 11 13 9 14
    return tempState

def invShiftRows(state):
    #state = 4x4 matrix representing block -- shift rows of state
    # copies each row per row in state -- copies each row list into a larger list to recreate matrix
    tempState = [row[:] for row in state]  #use tempState so state values do not get changed while rearranging
    for row in range(4):
        for col in range(4):
            tempState[row][col] = state[row][(col-row)%4] #values shift left based on row# 
    return tempState #replace state

def invSubBytes(state):
    #replace each byte w byte from inverse S-box
    #sBox rows and columns are organized 0-F
    invSBox = [
    [0x52, 0x09, 0x6A, 0xD5, 0x30, 0x36, 0xA5, 0x38, 0xBF, 0x40, 0xA3, 0x9E, 0x81, 0xF3, 0xD7, 0xFB],
    [0x7C, 0xE3, 0x39, 0x82, 0x9B, 0x2F, 0xFF, 0x87, 0x34, 0x8E, 0x43, 0x44, 0xC4, 0xDE, 0xE9, 0xCB],
    [0x54, 0x7B, 0x94, 0x32, 0xA6, 0xC2, 0x23, 0x3D, 0xEE, 0x4C, 0x95, 0x0B, 0x42, 0xFA, 0xC3, 0x4E],
    [0x08, 0x2E, 0xA1, 0x66, 0x28, 0xD9, 0x24, 0xB2, 0x76, 0x5B, 0xA2, 0x49, 0x6D, 0x8B, 0xD1, 0x25],
    [0x72, 0xF8, 0xF6, 0x64, 0x86, 0x68, 0x98, 0x16, 0xD4, 0xA4, 0x5C, 0xCC, 0x5D, 0x65, 0xB6, 0x92],
    [0x6C, 0x70, 0x48, 0x50, 0xFD, 0xED, 0xB9, 0xDA, 0x5E, 0x15, 0x46, 0x57, 0xA7, 0x8D, 0x9D, 0x84],
    [0x90, 0xD8, 0xAB, 0x00, 0x8C, 0xBC, 0xD3, 0x0A, 0xF7, 0xE4, 0x58, 0x05, 0xB8, 0xB3, 0x45, 0x06],
    [0xD0, 0x2C, 0x1E, 0x8F, 0xCA, 0x3F, 0x0F, 0x02, 0xC1, 0xAF, 0xBD, 0x03, 0x01, 0x13, 0x8A, 0x6B],
    [0x3A, 0x91, 0x11, 0x41, 0x4F, 0x67, 0xDC, 0xEA, 0x97, 0xF2, 0xCF, 0xCE, 0xF0, 0xB4, 0xE6, 0x73],
    [0x96, 0xAC, 0x74, 0x22, 0xE7, 0xAD, 0x35, 0x85, 0xE2, 0xF9, 0x37, 0xE8, 0x1C, 0x75, 0xDF, 0x6E],
    [0x47, 0xF1, 0x1A, 0x71, 0x1D, 0x29, 0xC5, 0x89, 0x6F, 0xB7, 0x62, 0x0E, 0xAA, 0x18, 0xBE, 0x1B],
    [0xFC, 0x56, 0x3E, 0x4B, 0xC6, 0xD2, 0x79, 0x20, 0x9A, 0xDB, 0xC0, 0xFE, 0x78, 0xCD, 0x5A, 0xF4],
    [0x1F, 0xDD, 0xA8, 0x33, 0x88, 0x07, 0xC7, 0x31, 0xB1, 0x12, 0x10, 0x59, 0x27, 0x80, 0xEC, 0x5F],
    [0x60, 0x51, 0x7F, 0xA9, 0x19, 0xB5, 0x4A, 0x0D, 0x2D, 0xE5, 0x7A, 0x9F, 0x93, 0xC9, 0x9C, 0xEF],
    [0xA0, 0xE0, 0x3B, 0x4D, 0xAE, 0x2A, 0xF5, 0xB0, 0xC8, 0xEB, 0xBB, 0x3C, 0x83, 0x53, 0x99, 0x61],
    [0x17, 0x2B, 0x04, 0x7E, 0xBA, 0x77, 0xD6, 0x26, 0xE1, 0x69, 0x14, 0x63, 0x55, 0x21, 0x0C, 0x7D]
]
    #replace each byte w byte from S-box
    for row in range(4): #upper nibble
        for col in range(4): #lower nibble
            #separate nibbles of each byte to check for row and column substitution
            #use masks to isolate nibbles
            sBoxRow = (state[row][col] & 0xF0) >>4 #must shift right to get just the value of the upper nibble
            sBoxCol = state[row][col] & 0x0F
            #substitute based on look up of each nibble of the value + the corresponding value in s-box
            state[row][col] = invSBox[sBoxRow][sBoxCol]


# encryption functions ------------------------------

def addRoundKey(state, rk):
    #XOR state matrix w roundKey matrix
    for row in range(4):
        for col in range(4):
            state[row][col] = state[row][col] ^ rk[row][col]

#Galois Field multiplication for mixColumns
def gfMult(byte, stateVal):
    mult = 0 #keeps track of product result
    #check each byte to multiply 
    for _ in range(8):
        # byte AND 00000001 -- would reveal if lowest bit 1
        if byte & 0x01:
            #then expression is not 0 -- add (XOR) x^k * stateVal(x)
            mult ^= stateVal

        #shift all bits left by 1 to multiply by x
        stateVal <<= 1 
        stateVal & 0xff #multiply from 0, x, x^2, .. to x^7

        #check stateVal highest bit (x^8) for overflow -- AND 100000000
        if stateVal & 0x100:
            # XOR with 0x11B in order to reduce
            # x^8 = x^4 + x^3 + x + 1
            # 0x11B = 100011011 = x^8 + x^4 + x^3 + x + 1
            # this should remove the 9th bit and replace w x^4 + x^3 + x + 1 = x^8
            stateVal ^= 0x11b

        #continue to next bit of byte
        byte >>= 1
    return mult & 0xff #just in case to keep 8 bits

def mixColumns(state):
    #each column = column x fixed 4x4 matrix of ints
    # copies each row per row in state -- copies each row list into a larger list to recreate matrix
    tempState = [row[:] for row in state]
    # iterate through each column
    for i in range(4): #change each value down the column using GF multiplication
        tempState[0][i] = gfMult(0x02, state[0][i]) ^ gfMult(0x03, state[1][i]) ^ state[2][i] ^ state[3][i] # 2 3 1 1 matrix to multiply against each column
        tempState[1][i] = state[0][i] ^ gfMult(0x02, state[1][i]) ^ gfMult(0x03, state[2][i]) ^ state[3][i] # 1 2 3 1
        tempState[2][i] = state[0][i] ^ state[1][i] ^ gfMult(0x02, state[2][i]) ^ gfMult(0x03, state[3][i]) # 1 1 2 3
        tempState[3][i] = gfMult(0x03, state[0][i]) ^ state[1][i] ^ state[2][i] ^ gfMult(0x02, state[3][i]) # 3 1 1 2
    return tempState

def shiftRows(state):
    #state = 4x4 matrix representing block -- shift rows of state
    # copies each row per row in state -- copies each row list into a larger list to recreate matrix
    tempState = [row[:] for row in state]  #use tempState so state values do not get changed while rearranging
    for row in range(4):
        for col in range(4):
            tempState[row][col] = state[row][(row+col)%4] #values shift left based on row# 
    #replace state
    return tempState

def subBytes(state):
    #sBox rows and columns are organized 0-F
    sBox = [
        [0x63, 0x7c, 0x77, 0x7b, 0xf2, 0x6b, 0x6f, 0xc5, 0x30, 0x01, 0x67, 0x2b, 0xfe, 0xd7, 0xab, 0x76],
        [0xca, 0x82, 0xc9, 0x7d, 0xfa, 0x59, 0x47, 0xf0, 0xad, 0xd4, 0xa2, 0xaf, 0x9c, 0xa4, 0x72, 0xc0],
        [0xb7, 0xfd, 0x93, 0x26, 0x36, 0x3f, 0xf7, 0xcc, 0x34, 0xa5, 0xe5, 0xf1, 0x71, 0xd8, 0x31, 0x15],
        [0x04, 0xc7, 0x23, 0xc3, 0x18, 0x96, 0x05, 0x9a, 0x07, 0x12, 0x80, 0xe2, 0xeb, 0x27, 0xb2, 0x75],
        [0x09, 0x83, 0x2c, 0x1a, 0x1b, 0x6e, 0x5a, 0xa0, 0x52, 0x3b, 0xd6, 0xb3, 0x29, 0xe3, 0x2f, 0x84],
        [0x53, 0xd1, 0x00, 0xed, 0x20, 0xfc, 0xb1, 0x5b, 0x6a, 0xcb, 0xbe, 0x39, 0x4a, 0x4c, 0x58, 0xcf],
        [0xd0, 0xef, 0xaa, 0xfb, 0x43, 0x4d, 0x33, 0x85, 0x45, 0xf9, 0x02, 0x7f, 0x50, 0x3c, 0x9f, 0xa8],
        [0x51, 0xa3, 0x40, 0x8f, 0x92, 0x9d, 0x38, 0xf5, 0xbc, 0xb6, 0xda, 0x21, 0x10, 0xff, 0xf3, 0xd2],
        [0xcd, 0x0c, 0x13, 0xec, 0x5f, 0x97, 0x44, 0x17, 0xc4, 0xa7, 0x7e, 0x3d, 0x64, 0x5d, 0x19, 0x73],
        [0x60, 0x81, 0x4f, 0xdc, 0x22, 0x2a, 0x90, 0x88, 0x46, 0xee, 0xb8, 0x14, 0xde, 0x5e, 0x0b, 0xdb],
        [0xe0, 0x32, 0x3a, 0x0a, 0x49, 0x06, 0x24, 0x5c, 0xc2, 0xd3, 0xac, 0x62, 0x91, 0x95, 0xe4, 0x79],
        [0xe7, 0xc8, 0x37, 0x6d, 0x8d, 0xd5, 0x4e, 0xa9, 0x6c, 0x56, 0xf4, 0xea, 0x65, 0x7a, 0xae, 0x08],
        [0xba, 0x78, 0x25, 0x2e, 0x1c, 0xa6, 0xb4, 0xc6, 0xe8, 0xdd, 0x74, 0x1f, 0x4b, 0xbd, 0x8b, 0x8a],
        [0x70, 0x3e, 0xb5, 0x66, 0x48, 0x03, 0xf6, 0x0e, 0x61, 0x35, 0x57, 0xb9, 0x86, 0xc1, 0x1d, 0x9e],
        [0xe1, 0xf8, 0x98, 0x11, 0x69, 0xd9, 0x8e, 0x94, 0x9b, 0x1e, 0x87, 0xe9, 0xce, 0x55, 0x28, 0xdf],
        [0x8c, 0xa1, 0x89, 0x0d, 0xbf, 0xe6, 0x42, 0x68, 0x41, 0x99, 0x2d, 0x0f, 0xb0, 0x54, 0xbb, 0x16]
    ]
    #replace each byte w byte from S-box
    for row in range(4): #upper nibble
        for col in range(4): #lower nibble
            #separate nibbles of each byte to check for row and column substitution
            #use masks to isolate nibbles
            sBoxRow = (state[row][col] & 0xF0) >>4 #must shift right to get just the value of the upper nibble
            sBoxCol = state[row][col] & 0x0F
            
            #substitute based on look up of each nibble of the value + the corresponding value in s-box
            state[row][col] = sBox[sBoxRow][sBoxCol]


# decode/encode functions ---------------------------

#want to convert flat block list of 16 bytes into 4x4 matrix
def blockStateConvert(block):
    # go through each column in each row 
    # 0   4   8   12   matrix should be organized like this if block list is 0-15
    # 1   5   9   13
    # 2   6   10  14
    # 3   7   11  15
    # by accessing the block index row + 4*col per col per row will arrange
    # row is used to increment by 1 -- each column increments in byte index downwards
    # column jumps by 4
    state = [[block[row + 4 * col] for col in range(4)] for row in range(4)]
    return state

#want to convert into 4x4 matrix into flat block list of 16 bytes
def stateBlockConvert(state):
    #one list converted to bytes
    # go through each row within each column : 0-3, 4-7, etc
    block = bytes([state[row][col] for col in range(4) for row in range(4)])
    return block


def decode(block, key, mode):
    roundKeys = keyExpansion(key, mode) #get list of roundkeys to use with addRoundKey
    state = blockStateConvert(block)
    #reverse the roundkeys order
    rkInd = 10
    if mode == "192":
        rkInd = 12
    if mode == "256":
        rkInd = 14
    addRoundKey(state, roundKeys[rkInd]) 
    for i in range(rkInd-1, 0, -1): #perform rounds-1 times reversed, increment -1
        state = invShiftRows(state)
        invSubBytes(state)
        addRoundKey(state, roundKeys[i])
        state = invMixColumns(state)
    #10th repetition
    state = invShiftRows(state)
    invSubBytes(state)
    addRoundKey(state, roundKeys[0])
    #convert state to block
    block = stateBlockConvert(state)
    return block


def encode(block, key, mode):
    roundKeys = keyExpansion(key, mode) #get list of roundkeys to use with addRoundKey
    #convert block to state (matrix) then continue
    state = blockStateConvert(block)
    rkInd = 10
    if mode == "192":
        rkInd = 12
    if mode == "256":
        rkInd = 14
    addRoundKey(state, roundKeys[0])
    for i in range(rkInd-1): # perform round-1 times
        subBytes(state)
        state = shiftRows(state)
        state = mixColumns(state)
        addRoundKey(state, roundKeys[i+1])
    #10th repetition
    subBytes(state)
    state = shiftRows(state)
    addRoundKey(state, roundKeys[rkInd])
    #revert state to block
    block = stateBlockConvert(state)
    return block


#decrypt/encrypt functions ---------------------------

def decrypt (encFile, k, mode):
    with open(k, "r") as file:
        key = file.readline().strip() #read key and remove newline
        keyBytes = bytes.fromhex(key) #key in byte format
        kList = list(keyBytes) #list of key bytes to help w rk generation

    blockList = []
    with open(encFile, "r") as file:
        strBlockList = file.readlines() #reads all lines into a list
        for line in strBlockList:
            hexStr = line.strip() #remove newlines
            block = bytes.fromhex(hexStr)
            blockList.append(block) #list of byte blocks

    concatBlocks = []
    #decrypt each block and concatenate
    for block in blockList:
        decBlock = decode(block, kList, mode) #decode each block
        concatBlocks.append(decBlock) #add each byte block into list
        decData = b''.join(concatBlocks) #concatenate the byte blocks into string of bytes

    hexDecData = decData.hex() #convert byte data to hex data
    with open("inputFile.enc.dec", "w") as file: #write to decrypted output file
        #i jump 32 every iteration -- write every 32 hex char on new line
        for i in range(0, len(hexDecData), 32):
            file.write(hexDecData[i:i+32] + "\n")


# one line keyfile; mult line inputfile
def encrypt (input, k, mode):
    with open(k, "r") as file:
        key = file.readline().strip() #read key and remove newline
        keyBytes = bytes.fromhex(key) #key in byte format
        kList = list(keyBytes) #separates bytes into list to help w roundkey generation
    #read input file, separate each line, convert to bytes, and store as a block array; make list of blocks
    blockList = []
    with open(input, "r") as file:
        strBlockList = file.readlines() #reads all lines into a list
        for line in strBlockList:
            hexStr = line.strip() #remove newlines
            #if line is more than 32 char, ignore the rest
            if len(hexStr) > 32:
                #first 32 char
                hexStr = hexStr[:32]
            #if line is less than 32 char, pad with zeros
            elif len(hexStr) < 32:
                #left justified -- right padding until 32 char w 0's
                hexStr = hexStr.ljust(32, "0")
            block = bytes.fromhex(hexStr)
            blockList.append(block) #list of byte blocks

    concatBlocks = []
    #encrypt each block and concatenate
    for block in blockList:
        encBlock = encode(block, kList, mode) #encode each block
        hexEncBlock = encBlock.hex() #translate each block of bytes into hex
        concatBlocks.append(hexEncBlock) #add each 32 char hex block to list
        
    with open("inputFile.enc", "w") as file: #write to output file
        for hexBlock in concatBlocks:
            file.write(hexBlock + '\n') #write each 32 char hex block on each line


#user input----------------------------------
#user enters: e/d, keyFile, inputFile
task = sys.argv[1] #encode or decode
k = (sys.argv[2]) #keyfile
inputFile = sys.argv[3] #file to encode or decode
AESmode = sys.argv[4] #AES 128, 192, or 256

if task == "e": #encode
    encrypt(inputFile, k, AESmode) #call encode and pass in input file and keyfile

elif task == "d": #decode
    decrypt(inputFile, k, AESmode) #call decode