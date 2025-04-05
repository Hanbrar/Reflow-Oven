# Reflow Oven Controller – ELEC 291 Project 1

![Project Badge](https://img.shields.io/badge/UBC-ELEC291-blue)

## 📽️ Summary Video

Check out our full video walkthrough on YouTube:  
👉 [Watch the Project Summary Video](https://youtu.be/eBJrCMrgCw4)

---

## 📘 Overview

This project is a complete reflow oven controller system developed for ELEC 291 at UBC. It automates the surface mount soldering process using a household toaster oven and integrates hardware and software modules for precision control and monitoring.

Key components include:
- Finite State Machine (FSM) logic
- Temperature monitoring using thermocouple and LM335 sensor
- LCD and serial communication feedback
- Real-time web-based interface
- PWM-based oven control
- Multiple safety features

---

## ⚙️ Features

- ✅ Accurate temperature control within ±3°C  
- ✅ Adjustable soak and reflow parameters using onboard buttons  
- ✅ Real-time temperature display on LCD and web UI  
- ✅ Graphical monitoring of reflow profile via Chart.js  
- ✅ CSV logging of each oven cycle  
- ✅ Manual stop button and automatic safety shutdowns  
- ✅ Audible buzzer alerts on state changes  

---

## 🧩 Hardware Components

- Nuvoton N76E003 Microcontroller  
- LM335 Temperature Sensor  
- K-type Thermocouple  
- OP07 Operational Amplifier  
- LMC7660 Voltage Converter  
- Solid State Relay (SSR)  
- 16x2 LCD Display  
- 5x Push Buttons  
- N-Channel MOSFET  
- Buzzer  

---

## 💻 Software Stack

- Assembly code for FSM, PWM, ADC, and UART  
- Web Interface:
  - HTML / CSS / JavaScript  
  - Web Serial API  
  - Chart.js for graphing  
- Python scripts using `matplotlib` for post-cycle analysis  
- GitHub and Visual Studio for version control and collaboration  

---

## 🌐 Web Interface Capabilities

- Serial connection to microcontroller using Web Serial API  
- Live plotting of oven temperature with colored phase indicators  
- Start/stop controls for reflow cycle  
- Estimated time remaining based on data trends  
- CSV export of historical cycle data  

---

## 📊 Data Handling

- Data transmitted via UART in ASCII  
- Logged fields: Time (s), Temperature (°C), Reflow State  
- Live data shown in-browser using Chart.js  
- Option to download data for offline analysis with Python  

---

## 🛡️ Safety Features

- Stops the process if:
  - Temperature remains <50°C after 60 seconds  
  - Manual stop button is pressed  
- Buzzer sound on each state transition  
- Real-time status indicators on LCD and web interface  

---

## 🤝 Collaborators

- Hanryck Brar  
- Guocheng Cao  
- Gavin Gao  
- Peter Gao  
- Jamie Kang  
- Jacob Park  

--- 



---

## 📚 References

- UBC ELEC 291 Course Materials  
- Nuvoton N76E003 Datasheet  
- Texas Instruments LM335, OP07, LMC7660  
- [Reflow Soldering – Wikipedia](https://en.wikipedia.org/wiki/Reflow_soldering)

---

## 🚀 How to Run

1. Upload assembly code to the N76E003 microcontroller.  
2. Connect the system to your computer via USB serial.  
3. Open the provided web interface in Chrome (Web Serial API supported).  
4. Connect to the microcontroller and configure reflow parameters.  
5. Press start to begin the reflow process and monitor live updates.

---
