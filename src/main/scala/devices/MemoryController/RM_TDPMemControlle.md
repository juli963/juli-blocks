# True Dual Port Mem Controller
**Parameters**  
|Parameter| Description|
|---|---|
|Address|Busaddress|
|regBytes |Buswidth in Bytes|
|sizeBytes|Size of BRAM in Bytes|
|useAXI4|When True then Mem is connected over AXI
  
  
**Registermap**  
|Offset  |Name  |
|----|----|
|0x00 | Write Control |
|0x04 | Write Address |  
|0x08 | Write Data |  
|0x0C | Read Address |  
|0x0F | Read Data |  
  
**Write Control**  
Bit 0 = Write Enable  

