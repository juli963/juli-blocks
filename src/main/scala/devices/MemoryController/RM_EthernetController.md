
# True Dual Port Mem Controller  

**Parameters**  
  
|Parameter| Description|
|---|---|
|Address|Busaddress|
|regBytes |Buswidth in Bytes|
|sizeBytes|Size of 1 Buffer in Bytes|
|numBuffers|Number of Buffers|
|useAXI4|When True then Mem is connected over AXI
  
**Registermap**  
Buffer Address 0 -> Length of Buffer RX/TX Data  
|Offset |Name |
|----|----|
|0x00| PHY Control|
|0x04 | TX Control |
|0x08 | TX Buffer Address |
|0x0C | TX Buffer Data |
|0x10 | RX Status |
|0x14 | RX Buffer Address |
|0x18 | RX Buffer Data |
  
**PHY Control**    
|Bit #|Name |
|----|----|
|0 | Link Established |
|1 | Half or Full Duplex | 
|7 - 4| Link Speed |
  
**TX Control**   
|Bit #|Name |
|----|----|
|0 | Write Enable |
|1 | Start Transmission (RTrig) |
|2 | Transmission in Progress |
|15-7 | Buffer Select |
  
**RX Status**  
|Bit #|Name |
|----|----|
|0 | Receive in Progress |
|1 | Interrupt Bit |
|2 | Buffer Lock |
|15-7 | Buffer Status (Bit Set means Buffer Full) |
|24-16| Buffer Select (Buffer # will get locked when Lock 1)|