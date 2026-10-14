# ==============================================================================
# Supplementary Material: python script
#
# Article:
# Remote Laboratories in Undergraduate Differential Equations:
# a pretest–posttest study in distance higher education
#
# Journal:
# Mathematics Education Research Journal (Springer Nature)
#
# Purpose:
# The purpose of this script is to display the results of the Likert scale.
#
# Note:
# Documentation comments have been edited and translated into English for supplementary publication.
# ==============================================================================

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.patches import Patch
import os

# Create the output directory for saving images
output_dir = "./images"
os.makedirs(output_dir, exist_ok=True)

# Load the three CSV files for their respective academic terms
df_is2024 = pd.read_csv("IS-2024.csv")
df_is2025 = pd.read_csv("IS-2025.csv")
df_iis2025 = pd.read_csv("IIS-2025.csv")

# Add a column identifying the academic term for each individual DataFrame
df_is2024["period"] = "IS-2024"
df_is2025["period"] = "IS-2025"
df_iis2025["period"] = "IIS-2025"

# Combine all DataFrames into a single DataFrame for unified processing
df = pd.concat([df_is2024, df_is2025, df_iis2025], ignore_index=True)

# Define the question groups and their associated column indices
groups = {
    "Tool Usability": [0, 1, 2, 3, 4, 5, 6, 19],
    "Learning Perception": [8, 9, 10, 11, 12, 13, 19],
    "User Satisfaction": [15, 16, 17, 18, 19],
}

# Possible Likert-scale values and their corresponding colors
likert_values = [1, 2, 3, 4, 5]
likert_colors = {
    1: "#d41515",
    2: "#e38989",
    3: "#9f9f9f",
    4: "#a2c7f5",
    5: "#1100f9",
}

# Convert a series of responses into percentages for each Likert-scale value
def likert_percent(series):
    vc = series.value_counts()
    counts = np.array([vc.get(k, 0) for k in likert_values], dtype=float)
    return 100 * counts / counts.sum() if counts.sum() else np.zeros(5)

def stats_text(series):
    """
    Return text in the format: Me=#, Mo=#, X=#
    - Me: median
    - Mo: mode (use the first value if there are multiple modes)
    - X : mean
    """
    if series is None or len(series) == 0:
        return "Me=NA, Mo=NA, X=NA"

    me = float(series.median())
    mo_series = series.mode()
    mo = float(mo_series.iloc[0]) if len(mo_series) else np.nan
    x = float(series.mean())

    # Format integers without decimal places; otherwise, use one decimal place
    def fmt(v):
        if np.isnan(v):
            return "NA"
        return str(int(v)) if abs(v - round(v)) < 1e-9 else f"{v:.1f}"

    return f"Me={fmt(me)}, Mo={fmt(mo)}, X={fmt(x)}"


# Draw a horizontal bar with colored segments for each Likert response category
def draw_likert_block(ax, y, perc, colors, label=None, left_text=None, show_percent=True, min_width=5):
    """
    Draw a diverging horizontal Likert bar and optionally display the percentage
    centered within each segment.
    - min_width: minimum width (in percentage points) for displaying text without crowding.
    """
    p1, p2, p3, p4, p5 = perc
    center = -p3 / 2  # Centered on the neutral (gray) segment

    # Left positions of each segment
    left_neutral = center
    left_p2 = center - p2
    left_p1 = center - p2 - p1
    left_p4 = center + p3
    left_p5 = center + p3 + p4

    # Draw each segment with white borders
    ax.barh(y, p3, left=left_neutral, height=0.2, color=colors[3], edgecolor='white', linewidth=1)  # Neutral
    ax.barh(y, p2, left=left_p2, height=0.2, color=colors[2], edgecolor='white', linewidth=1)      # Partially disagree
    ax.barh(y, p1, left=left_p1, height=0.2, color=colors[1], edgecolor='white', linewidth=1)      # Strongly disagree
    ax.barh(y, p4, left=left_p4, height=0.2, color=colors[4], edgecolor='white', linewidth=1)      # Partially agree
    ax.barh(y, p5, left=left_p5, height=0.2, color=colors[5], edgecolor='white', linewidth=1)      # Strongly agree

    # Percentage labels inside each segment
    if show_percent:
        # Text colors for contrast
        text_colors = {1: "white", 2: "black", 3: "black", 4: "black", 5: "white"}
        segments = [
            (p1, left_p1, 1),
            (p2, left_p2, 2),
            (p3, left_neutral, 3),
            (p4, left_p4, 4),
            (p5, left_p5, 5),
        ]
        for width, left, k in segments:
            if width and width >= min_width:
                ax.text(
                    left + width / 2,
                    y,
                    f"{width:.0f}%",
                    ha="center",
                    va="center",
                    fontsize=8,
                    color=text_colors.get(k, "black"),
                )

    #if left_text: # This line displays the summary statistics
    #    ax.text(-110, y, left_text, ha="left", va="center", fontsize=9, clip_on=False)

    # Period label on the right
    if label:
        ax.text(-100, y, label, ha="left", va="center", fontsize=10)

# Generate a plot for each group of questions
def plot_likert_group(group_title, col_idxs):
    # Select the relevant columns and rename them
    df_group = df.iloc[:, col_idxs].copy()
    df_group.columns = list(df.columns[col_idxs[:-1]]) + ["period"]

    questions = df_group.columns[:-1]  # All columns except 'period'
    periods = ["IS-2024", "IS-2025", "IIS-2025", "Total"]

    # Assign question codes based on the group
    prefix_map = {
        "User Satisfaction": "S",
        "Tool Usability": "U",
        "Learning Perception": "L",
    }
    prefix = prefix_map.get(group_title, "Q")  # fallback: Q1, Q2, ...
    statements = [f"{prefix}{i+1}" for i in range(len(questions))]

    # Calculate plot height based on the number of questions and the space required for text
    line_height = 0.25
    question_heights = []
    for q in statements:
        lines = int(np.ceil(len(q) / 100))
        height = lines * line_height + len(periods) * 0.2
        question_heights.append(height)

    total_height = sum(question_heights) + 2

    # Initialize the figure
    fig, ax = plt.subplots(figsize=(12, total_height))
    ax.set_xlim(-120, 120)
    ax.axis("off")

    # Initial vertical position for text
    y = total_height - 1

    # Iterate over the questions
    for idx, question in enumerate(questions):
        statement = statements[idx]
        lines = int(np.ceil(len(statement) / 85))
        ax.text(0, y-0.07, statement, ha="center", va="bottom", fontsize=9, fontweight="bold", wrap=True)
        y -= lines * line_height

        for period in periods:
            if period == "Total":
                data = df_group[question].dropna()
            else:
                data = df_group[df_group["period"] == period][question].dropna()

            data = data.astype(str).str.strip().astype(float)
            data = data[data.isin(likert_values)]
            perc = likert_percent(data)
            
            stats = stats_text(data)
            draw_likert_block(ax, y, perc, likert_colors, label=period, left_text=stats)

            #draw_likert_block(ax, y, perc, likert_colors, label=period)
            y -= 0.25

    # Center line and percentage labels
    ax.axvline(0, color="white", linewidth=1, alpha = 0)
    for x in [-100, -50, 0, 50, 100]:
        ax.text(x, -0.4, f"{abs(x)}%", ha="center", va="top", fontsize=9)

    ## Plot title
    #ax.text(0, total_height - 0.2, f"Likert Distribution for {group_title}", ha="center", fontsize=16, fontweight="bold")

    # Color legend
    legend_items = [
        Patch(facecolor=likert_colors[1], label="Strongly disagree"),
        Patch(facecolor=likert_colors[2], label="Disagree"),
        Patch(facecolor=likert_colors[3], label="Neither agree nor disagree"),
        Patch(facecolor=likert_colors[4], label="Agree"),
        Patch(facecolor=likert_colors[5], label="Strongly agree"),
    ]

    # Default settings for all plots
    legend_y = 0
    tight_bottom = 0.02

    # Adjustment ONLY for this figure
    if group_title == "User Satisfaction":
        legend_y = 0  # Lower the legend slightly
        tight_bottom = 0.04   # Leave more space at the bottom


    #fig.legend(handles=legend_items, loc="lower center", bbox_to_anchor=(0.5, 0), ncol=5, frameon=False)
    #fig.tight_layout(rect=[0, 0.02, 1, 1])
    fig.legend(handles=legend_items, loc="lower center", bbox_to_anchor=(0.5, legend_y), ncol=5, frameon=False)
    fig.tight_layout(rect=[0, tight_bottom, 1, 1])

    # Save the image to the output directory
    fig.savefig(f"{output_dir}/likert_{group_title}.png", dpi=300)
    plt.close(fig)

# Generate plots for each defined group
for title, col_idxs in groups.items():
    plot_likert_group(title, col_idxs)

