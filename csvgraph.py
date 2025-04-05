import pandas as pd
import matplotlib.pyplot as plt

# Read CSV file
df = pd.read_csv('oven_cycle_1.csv')

# Create figure and axis
fig, ax = plt.subplots(figsize=(12, 6))

# Plot temperature over time
ax.plot(df['Time'], df['Temperature'], color='black', label='Temperature (°C)')

# Define colors for each state
state_colors = {
    'Ramp To Soak': 'yellow',
    'Ramp To Peak': 'orange',
    'Preheat/Soak': 'red',
    'Reflow': 'purple',
    'Cooling': 'blue'
}

# Track state changes and shade regions
previous_state = df['State'].iloc[0]
start_time = df['Time'].iloc[0]

for i in range(1, len(df)):
    current_state = df['State'].iloc[i]
    if current_state != previous_state:
        end_time = df['Time'].iloc[i-1]
        # Shade the region for the previous state
        ax.axvspan(
            start_time, end_time,
            color=state_colors.get(previous_state, 'gray'),
            alpha=0.3,
            label=previous_state
        )
        previous_state = current_state
        start_time = df['Time'].iloc[i]

# Shade the last state region
end_time = df['Time'].iloc[-1]
ax.axvspan(
    start_time, end_time,
    color=state_colors.get(previous_state, 'gray'),
    alpha=0.1,
    label=previous_state
)

# Add labels and title
ax.set_xlabel('Time (seconds)')
ax.set_ylabel('Temperature (°C)')
ax.set_title('Oven Temperature Cycle with State Changes')
ax.grid(color='#524e4b')

# Add legend
handles, labels = ax.get_legend_handles_labels()
unique_labels = []
unique_handles = []
for handle, label in zip(handles, labels):
    if label not in unique_labels:
        unique_labels.append(label)
        unique_handles.append(handle)
ax.legend(unique_handles, unique_labels, bbox_to_anchor=(1.05, 1), loc='upper left')

# Save the plot as an image (optional)
plt.tight_layout()
plt.savefig('oven_cycle_plot.png', dpi=300)

# Display the plot
plt.show()
