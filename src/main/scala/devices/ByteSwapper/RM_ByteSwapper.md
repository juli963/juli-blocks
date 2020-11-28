
# Byte Swapper 
  
**Parameters**  
|Parameter| Description|
|---|---|
|inBytes|Number of Bytes as Input for swapping|
|regBytes |Buswidth in Bytes|
  
**Registermap**  
Example Registermap for inBytes = 8 and regBytes = 4
|Offset |Name |
|----|----|
|0x00| Input Lower Bytes | 
|0x04 | Input Higher Bytes | 
|0x08 | 2 Bytes Swapped (16Bit) | 
|0x0C | 3 Bytes Swapped (24Bit) | 
|0x10 | 4 Bytes Swapped (32Bit) | 
|0x14 | 5 Bytes Swapped Low (40Bit) | 
|0x18 | 5 Bytes Swapped High (40Bit) | 
|0x1C | 6 Bytes Swapped Low (48Bit) | 
|0x20 | 6 Bytes Swapped High (48Bit) | 
|0x24 | 7 Bytes Swapped Low (56Bit) | 
|0x28 | 7 Bytes Swapped High (56Bit) | 
|0x2C | 8 Bytes Swapped Low (64Bit) | 
|0x30 | 8 Bytes Swapped High (64Bit) | 
