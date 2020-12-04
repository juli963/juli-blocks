
# Ethernet Controller
  
**Parameters**  
|Parameter| Description|
|---|---|
|address|Register Address (Registers)|
|regBytes |Buswidth in Bytes|
|sizeBytes|Size of 1 Buffer in Bytes|
|fAddress |Memory Address (Buffers)|
|numBuffers|Number of Buffers|
|sizeBuffer|Size of 1 Buffer|
|useAXI4|When True then Mem is connected over AXI -> Not supported yet|
  
**Registermap**  
Buffer Address 0 -> Length of Buffer RX/TX Data in Bytes, without Length Bytes, only Data (Size of 4 Bytes)  
|Offset |Name |
|----|----|
|0x00| PHY Control| 
|0x04 | TX Control | 
|0x08 | RX Status | 
|0x0C| MAC Filter Low Bytes|
|0x10| MAC Filter High Bytes|
|0x14-0x44| Byte Swapper|
  
**Memory Organization**
Buffer Address 0 -> Length of Buffer RX/TX Data in Bytes   
|Address |Name |
|----|----|
|address| Registerinterface| 
|fAddress | TX Buffers | 
|fAddress+(numBuffers*sizeBuffer) | RX Buffers | 

**PHY Control**    
|Bit #|Name |
|----|----|
|0 | Link Established |
|1 | Half or Full Duplex | 
|2-3 | Force Speed(Select Protocol 0=Auto, 1=MII, 2=RGMII) |
|7 - 4| Link Speed |
  
**TX Control**   
|Bit #|Name |
|----|----|
|0 | Reserved |
|1 | Start Transmission (RTrig) |
|2 | Transmission in Progress |
|15-8 | Buffer Select |
  
**RX Status**  
|Bit #|Name |
|----|----|
|0 | Receive in Progress |
|1 | Interrupt Bit |
|2 | Buffer Lock |
|15-8| Buffer Select (Buffer # will get locked when Lock 1)|
|24-16 | Buffer Status (Bit Set means Buffer Full) |

**MAC Filter** 
Filters incoming Packages according to the Destination MAC Address. Only Packages with Destination MAC = MAC Filter OR Destination MAC = Broadcast MAC are written into the Receive Buffer. 
Filtering will be inactive when MAC Filter = 0.