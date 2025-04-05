
import sys
import pandas as pd
if sys.version_info[0] < 3:
    import Tkinter
    from tkinter import *
    import tkMessageBox
else:
    import tkinter as Tkinter
    from tkinter import *
    from tkinter import messagebox as tkMessageBox
import time
import serial
import serial.tools.list_ports
import kconvert

# Initialize data collection list
data_list = []

top = Tk()
top.resizable(0,0)
top.title("Fluke_45/Tek_DMM40xx K-type Thermocouple")

#ATTENTION: Make sure the multimeter is configured at 9600 baud, 8-bits, parity none, 1 stop bit, echo Off

CJTemp = StringVar()
Temp = StringVar()
DMMout = StringVar()
portstatus = StringVar()
DMM_Name = StringVar()
connected=0
global ser
   
def Just_Exit():
    # Save collected data to CSV before exiting
    if data_list:
        df = pd.DataFrame(data_list)
        df.to_csv('thermocouple_data.csv', index=False)
    top.destroy()
    try:
        ser.close()
    except:
        dummy=0

def update_temp():
    global ser, connected
    if connected==0:
        top.after(5000, FindPort)
        return
    try:
        strin_bytes = ser.readline()
        strin=strin_bytes.decode()
        ser.readline()
        if len(strin)>1:
            if strin[1]=='>':
                strin_bytes = ser.readline()
                strin = strin_bytes.decode() 
        ser.write(b"MEAS1?\r\n")
    except:
        connected=0
        DMMout.set("----")
        Temp.set("----");
        portstatus.set("Communication Lost")
        DMM_Name.set ("--------")
        top.after(5000, FindPort)
        return
    strin_clean = strin.replace("VDC","")
    if len(strin_clean) > 0:      
       DMMout.set(strin.replace("\r", "").replace("\n", ""))

       try:
           val=float(strin_clean)*1000.0
           valid_val=1;
       except:
           valid_val=0

       try:
          cj=float(CJTemp.get())
       except:
          cj=0.0

       strin2 = ser2.readline()
       strin2 = strin2.rstrip()
       strin2 = strin2.decode()

       if len(strin2) > 0:
          try:
             val2=float(strin2)
             val2=val2/100
          except:
             val2=0

       if valid_val == 1 :
           ktemp=round(kconvert.mV_to_C(val, cj),1)
           if ktemp < -200:  
               Temp.set("UNDER")
           elif ktemp > 1372:
               Temp.set("OVER")
           else:
               Temp.set(ktemp)
               try:
                  print(ktemp, val2, round(abs(ktemp-val2),2))
                  # Store values in data list
                  data_list.append({
                      'ktemp': ktemp,
                      'val2': val2,
                      'difference': round(abs(ktemp-val2),2)
                  })
               except:
                  dummy=1
       else:
           Temp.set("----");
    else:
       Temp.set("----");
       connected=0;
    top.after(500, update_temp)

def FindPort():
   global ser, connected
   try:
       ser.close()
   except:
       dummy=0
       
   connected=0
   DMM_Name.set ("--------")
   portlist=list(serial.tools.list_ports.comports())
   for item in reversed(portlist):
      portstatus.set("Trying port " + item[0])
      top.update()
      try:
         ser = serial.Serial(item[0], 9600, timeout=0.5)
         time.sleep(0.2)
         ser.write(b'\x03')
         instr = ser.readline()
         pstring = instr.decode();
         if len(pstring) > 1:
            if pstring[1]=='>':
               ser.timeout=3
               portstatus.set("Connected to " + item[0])
               ser.write(b"VDC; RATE S; *IDN?\r\n")
               instr=ser.readline()
               devicename=instr.decode()
               DMM_Name.set(devicename.replace("\r", "").replace("\n", ""))
               ser.readline()
               ser.write(b"MEAS1?\r\n")
               connected=1
               top.after(1000, update_temp)
               break
            else:
               ser.close()
         else:
            ser.close()
      except:
         connected=0
   if connected==0:
      portstatus.set("Multimeter not found")
      top.after(5000, FindPort)

Label(top, text="Cold Junction Temperature:").grid(row=1, column=0)
Entry(top, bd =1, width=7, textvariable=CJTemp).grid(row=2, column=0)
Label(top, text="Multimeter reading:").grid(row=3, column=0)
Label(top, text="xxxx", textvariable=DMMout, width=20, font=("Helvetica", 20), fg="red").grid(row=4, column=0)
Label(top, text="Thermocouple Temperature (C)").grid(row=5, column=0)
Label(top, textvariable=Temp, width=5, font=("Helvetica", 100), fg="blue").grid(row=6, column=0)
Label(top, text="xxxx", textvariable=portstatus, width=40, font=("Helvetica", 12)).grid(row=7, column=0)
Label(top, text="xxxx", textvariable=DMM_Name, width=40, font=("Helvetica", 12)).grid(row=8, column=0)
Button(top, width=11, text = "Exit", command = Just_Exit).grid(row=9, column=0)

CJTemp.set ("22")
DMMout.set ("NO DATA")
DMM_Name.set ("--------")

port = 'COM9'  # Update this COM port if needed

try:
   ser2 = serial.Serial(port, 115200, timeout=0)
except:
   print('Serial port %s is not available' % (port))
   portlist=list(serial.tools.list_ports.comports())
   print('Available serial ports:')
   for item in portlist:
      print (item[0])

top.after(500, FindPort)
top.mainloop()