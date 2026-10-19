## **Variable View & Label Editor Help**

This menu allows you to manage how your data is structured, labeled, and interpreted by JASP. The sections below explain each setting and how to use it effectively.

### **Name, Long Name & Description**

* **Name:** A short name of the variable which will be shown in tables and along the axis of graphs.
* **Long Name:** Designed for reporting. *Note: Integration into output tables is a work in progress. In the future, this will allow JASP to automatically display long names in your output without cluttering your data view / spreadsheet.*
* **Description:** Space for notes, hypotheses, codebook definitions etc.

### **Types / Levels of Measurement**

* **Scale (Ruler icon):** Continuous numerical data (e.g., age, weight, test scores).
* **Ordinal (Staircase icon):** Categorical data with a logical, built-in progression (e.g., education level, Likert scales, tournament placement). *Note: JASP may initially import ordered numbers as Scale; simply click the icon to switch it to Ordinal.*
* **Nominal (Venn diagram icon):** Text/strings or categorical data without an inherent order (e.g., eye color, country, ID numbers).

### **Missing Values**

When a data cell is empty — whether because a measurement instrument failed or a participant skipped a question — it is treated as a **missing value**.

* By default, JASP recognizes empty cells as missing data if they contain:  
  NaN (not available number), NA, nan, . (period), blank space
* If your dataset uses a specific numeric code for missing data (such as \-99 or 999), you can add that code to the **Missing Values** list by activating **Use custom values**, entering them next to the +button and clicking the +button. JASP will then treat those entries as missing and exclude them from statistical calculations.

### **How JASP Imports Data**

When a dataset is imported, JASP automatically assigns a variable type based on the contents of the column.

If a column contains only numbers (excluding recognized missing values):

* **Nominal**: exactly 2 unique integer values  
* **Ordinal:** 3-10 unique integer values  
* **Scale:** 1 or more than 10 unique integer values, or at least 1 value containing a decimal.

If at least one of the values is not a number nor a missing value

* **Nominal:** 1-2 unique values or more than 10 unique values  
* **Ordinal:** 3 to 10 unique values

Some of these import settings can be customized via: ‘File → Preferences → Data → Import Settings’. There you can also change ‘**Missing Value List**’ and the ‘**Threshold for scale**’ (default 10).

### **Manually Changing Variable Types**

Types can be changed by clicking the variable type icon at most places in JASP. To do this for multiple variables at once, mark multiple columns in data edit view and change type once.

JASP will attempt to convert the variable to the selected type whenever possible. For example, if a variable is changed to **Scale** but contains non-numeric values, those values will be converted to missing values.

In some situations, variable types are enforced by an analysis or computed column. For example, a t-test requires the dependent variable to be treated as **Scale**. When JASP temporarily changes a variable type in this way, an asterisk (`*`) appears next to the type icon to indicate that the type has been overridden.

### **Computed Columns**

If a column was created using the Computed Column (+) formula builder, its editing behaviour depends on the variable type:

* **Scale Computed Columns:** The formula calculates the underlying numerical **values**. You may freely edit or add labels, but the calculated values themselves cannot be edited manually.  
* **Nominal / Ordinal Computed Columns:** The formula determines the **labels**. Therefore, you may edit the underlying numerical values, but the labels themselves are controlled by the computation and cannot be edited directly.

### **Level Manipulation & Ordering**

You can precisely control and order your data levels:

* **Use labels:** When working with massive datasets (tens of thousands of rows), rendering custom text labels can slow down performance. Toggling this off forces JASP to display raw numbers instead, significantly speeding up the interface. The **Use labels** toggle is disabled by default when importing data that contains no labels, or when creating a dataset from scratch. Enable it if you want to create, view, change, or sort labels.
* **Eye button:** Closed - Drops levels you have filtered or newly created from your result tables and graphs. Open - Includes filtered and newly created levels:
* **Automatically order labels by their value** *(1 to N arrow down button)*: Forces the labels to align strictly in ascending order based on their underlying numeric values.
* **Reverse order of all numerical values** *(1 to N circular arrows button)*: Inverts the numeric values assigned to your labels. (e.g., turns a scale of 1 \= Low, 3 \= High into 3 \= Low, 1 \= High).
* **Reverse order of all labels** *(Up/Down arrow button)*: Flips the text label order. *Note: If automatic ordering is enabled, it will be disabled automatically when this option is used.*
* **Move labels up manually** *(Up arrow button)*: Moves the selected label one position higher in the list. *(Turns off automatic ordering).*
* **Move labels down manually** *(Down arrow button)*: Moves the selected label one position lower in the list. *(Turns off automatic ordering).*
* **Reset all filter checkmarks** *(Erasor button)*: Only visible if you have filtered data in this column. Removes all filters from this variable if you have set them with the checkmark toggle.

💡 **Statistical Tip (Changing the Baseline):** The order of your levels matters deeply for advanced statistics like Regression or GLM. JASP uses the **very top level** in this list as the reference/baseline group for dummy coding. Dragging or moving a level to the top changes your model's statistical baseline\!

### **Filter, Value, Label, Remove, Add new level**

* **Filter (Check / Uncheck):** In this column, unchecking a level temporarily excludes all cases with that value from analyses. This provides a quick way to include or exclude groups without modifying the dataset itself.
* **Value:** This column shows a database of all different values within your raw data. The raw data and thus the values are used to compute the statistics, tables and graphs for the analyses you choose. If you change a value this replaces all values within that column with the new one you have set. This essentially is a "find and replace" functionality. You probably want to make a backup of your data (e.g. by copy pasting the column to a backup column next to it) befory you manipulate your data this way.
* **Label:** This column shows labels of your data values. They are used to make the axis of your graphs and also your tables more easily readable. They do not have an influence on the actual computations that produce your statistical results.
* **Remove (X / Delete):** Removes the selected value from this variable only and converts it to a missing value. All other data for that participant or case remain unchanged.
* **Add New Level (+):** Creates a category that does not currently appear in the data.
  * *Use Case:* Suppose a survey uses a response scale from 1 (*Strongly Disagree*) to 5 (*Strongly Agree*), but nobody selected 5 in the current sample. Because that level does not occur in the data, JASP will not display it automatically. Adding the level manually ensures that plots and summary tables show the complete intended response scale.
