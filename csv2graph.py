import os
import time
import pandas as pd
import matplotlib.pyplot as plt
from watchdog.observers import Observer
from watchdog.events import FileSystemEventHandler

# Enable interactive plotting mode
plt.ion()

# Define the directory to watch (Downloads)
WATCH_DIRECTORY = os.path.join(os.path.expanduser("~"), "Downloads")
print("Watching directory:", WATCH_DIRECTORY)

# Global variable to track the last plotted file
last_plotted_file = None

def get_latest_csv():
    """Returns the path to the most recently modified CSV file in the directory."""
    csv_files = [f for f in os.listdir(WATCH_DIRECTORY) if f.endswith('.csv')]
    if not csv_files:
        return None

    latest_csv = max(csv_files, key=lambda x: os.path.getmtime(os.path.join(WATCH_DIRECTORY, x)))
    return os.path.join(WATCH_DIRECTORY, latest_csv)

def plot_data():
    """Reads and plots the latest CSV file dynamically, updating in real-time."""
    global last_plotted_file

    while True:
        latest_csv = get_latest_csv()

        if latest_csv and latest_csv != last_plotted_file:
            print(f"New file detected: {latest_csv}")
            time.sleep(1)  # Ensure the file is fully written before reading

            try:
                df = pd.read_csv(latest_csv)
                print(f"Opened CSV successfully. Columns: {df.columns}")

                # Check if required columns exist
                required_columns = {'Time', 'Temperature', 'State'}
                if not required_columns.issubset(df.columns):
                    print("Error: Missing required columns:", required_columns - set(df.columns))
                    continue

                # Convert 'Time' column to numeric
                df['Time'] = pd.to_numeric(df['Time'], errors='coerce')
                df.dropna(subset=['Time'], inplace=True)

                # Clear the figure and plot new data
                plt.clf()
                plt.figure(1)

                # Plot temperature over time
                plt.plot(df['Time'], df['Temperature'], color='black', label='Temperature (°C)')

                # Define colors for each state
                state_colors = {
                    'Ramp To Soak': 'yellow',
                    'Ramp To Peak': 'orange',
                    'Preheat/Soak': 'red',
                    'Reflow': 'purple',
                    'Cooling': 'blue'
                }

                # Shade regions for different states
                previous_state = df['State'].iloc[0]
                start_time = df['Time'].iloc[0]

                for i in range(1, len(df)):
                    current_state = df['State'].iloc[i]
                    if current_state != previous_state:
                        end_time = df['Time'].iloc[i-1]
                        plt.axvspan(start_time, end_time, color=state_colors.get(previous_state, 'gray'), alpha=0.3, label=previous_state)
                        previous_state = current_state
                        start_time = df['Time'].iloc[i]

                # Shade the last state region
                end_time = df['Time'].iloc[-1]
                plt.axvspan(start_time, end_time, color=state_colors.get(previous_state, 'gray'), alpha=0.1, label=previous_state)

                # Add labels and title
                plt.xlabel('Time (seconds)')
                plt.ylabel('Temperature (°C)')
                plt.title('Oven Temperature Cycle with State Changes')
                plt.grid(color='#524e4b')

                # Add legend with unique labels
                handles, labels = plt.gca().get_legend_handles_labels()
                unique_labels = []
                unique_handles = []
                for handle, label in zip(handles, labels):
                    if label not in unique_labels:
                        unique_labels.append(label)
                        unique_handles.append(handle)
                plt.legend(unique_handles, unique_labels, bbox_to_anchor=(1.05, 1), loc='upper left')

                # Draw the plot and pause for updates
                plt.draw()
                plt.pause(0.1)

                # Update the last plotted file
                last_plotted_file = latest_csv

            except Exception as e:
                print(f"Error reading CSV file: {e}")

        time.sleep(1)  # Check for new files every second

class WatcherHandler(FileSystemEventHandler):
    """Handles new file events and triggers re-plotting."""
    def on_created(self, event):
        if event.src_path.endswith('.csv'):
            print(f"New CSV detected: {event.src_path}")
            plot_data()  # Update the plot dynamically

# Start observer to monitor the directory
observer = Observer()
handler = WatcherHandler()
observer.schedule(handler, WATCH_DIRECTORY, recursive=False)

observer.start()

# Run dynamic plotting in a separate thread to keep updating the latest file
print("Starting dynamic plot...")
plot_data()

try:
    while True:
        time.sleep(1)
except KeyboardInterrupt:
    observer.stop()
observer.join()