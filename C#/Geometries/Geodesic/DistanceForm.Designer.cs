namespace Geometries;

partial class DistanceForm {
  /// <summary>
  ///  Required designer variable.
  /// </summary>
  private System.ComponentModel.IContainer components = null;

  /// <summary>
  ///  Clean up any resources being used.
  /// </summary>
  /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
  protected override void Dispose(bool disposing) {
    if (disposing && (components != null)) {
      components.Dispose();
    }
    base.Dispose(disposing);
  }

  #region Windows Form Designer generated code

  /// <summary>
  ///  Required method for Designer support - do not modify
  ///  the contents of this method with the code editor.
  /// </summary>
  private void InitializeComponent() {
    tboxLatt1 = new TextBox();
    tboxLong1 = new TextBox();
    tboxLatt2 = new TextBox();
    tboxLong2 = new TextBox();
    tboxDist = new TextBox();
    lblLatt1 = new Label();
    lblLong1 = new Label();
    lblLatt2 = new Label();
    lblLong2 = new Label();
    lblDist = new Label();
    btnCalc = new Button();
    groupUnits = new GroupBox();
    radioUnitsKM = new RadioButton();
    radioUnitsMiles = new RadioButton();
    groupUnits.SuspendLayout();
    SuspendLayout();
    // 
    // tboxLatt1
    // 
    tboxLatt1.Location = new Point(60, 64);
    tboxLatt1.Name = "tboxLatt1";
    tboxLatt1.Size = new Size(146, 20);
    tboxLatt1.TabIndex = 0;
    // 
    // tboxLong1
    // 
    tboxLong1.Location = new Point(280, 64);
    tboxLong1.Name = "tboxLong1";
    tboxLong1.Size = new Size(146, 20);
    tboxLong1.TabIndex = 1;
    // 
    // tboxLatt2
    // 
    tboxLatt2.Location = new Point(60, 126);
    tboxLatt2.Name = "tboxLatt2";
    tboxLatt2.Size = new Size(146, 20);
    tboxLatt2.TabIndex = 2;
    // 
    // tboxLong2
    // 
    tboxLong2.Location = new Point(280, 126);
    tboxLong2.Name = "tboxLong2";
    tboxLong2.Size = new Size(146, 20);
    tboxLong2.TabIndex = 3;
    // 
    // tboxDist
    // 
    tboxDist.Location = new Point(60, 236);
    tboxDist.Name = "tboxDist";
    tboxDist.Size = new Size(146, 20);
    tboxDist.TabIndex = 4;
    // 
    // lblLatt1
    // 
    lblLatt1.AutoSize = true;
    lblLatt1.Location = new Point(60, 46);
    lblLatt1.Name = "lblLatt1";
    lblLatt1.Size = new Size(31, 13);
    lblLatt1.TabIndex = 5;
    lblLatt1.Text = "Latt1";
    // 
    // lblLong1
    // 
    lblLong1.AutoSize = true;
    lblLong1.Location = new Point(280, 46);
    lblLong1.Name = "lblLong1";
    lblLong1.Size = new Size(37, 13);
    lblLong1.TabIndex = 6;
    lblLong1.Text = "Long1";
    // 
    // lblLatt2
    // 
    lblLatt2.AutoSize = true;
    lblLatt2.Location = new Point(60, 108);
    lblLatt2.Name = "lblLatt2";
    lblLatt2.Size = new Size(31, 13);
    lblLatt2.TabIndex = 7;
    lblLatt2.Text = "Latt2";
    // 
    // lblLong2
    // 
    lblLong2.AutoSize = true;
    lblLong2.Location = new Point(280, 108);
    lblLong2.Name = "lblLong2";
    lblLong2.Size = new Size(37, 13);
    lblLong2.TabIndex = 8;
    lblLong2.Text = "Long2";
    // 
    // lblDist
    // 
    lblDist.AutoSize = true;
    lblDist.Location = new Point(60, 222);
    lblDist.Name = "lblDist";
    lblDist.Size = new Size(47, 13);
    lblDist.TabIndex = 9;
    lblDist.Text = "distance";
    // 
    // btnCalc
    // 
    btnCalc.Location = new Point(60, 176);
    btnCalc.Name = "btnCalc";
    btnCalc.Size = new Size(58, 21);
    btnCalc.TabIndex = 10;
    btnCalc.Text = "=";
    btnCalc.UseVisualStyleBackColor = true;
    btnCalc.Click += new System.EventHandler(btnCalc_Click);
    // 
    // radioUnitsKM
    // 
    radioUnitsKM.AutoSize = true;
    radioUnitsKM.Location = new Point(6, 19);
    radioUnitsKM.Name = "radioUnitsKM";
    radioUnitsKM.Size = new Size(39, 17);
    radioUnitsKM.TabIndex = 11;
    radioUnitsKM.Text = "km";
    radioUnitsKM.UseVisualStyleBackColor = true;
    radioUnitsKM.CheckedChanged += new System.EventHandler(radioUnitsKM_CheckedChanged);
    // 
    // radioUnitsMiles
    // 
    radioUnitsMiles.AutoSize = true;
    radioUnitsMiles.Checked = true;
    radioUnitsMiles.Location = new Point(6, 47);
    radioUnitsMiles.Name = "radioUnitsMiles";
    radioUnitsMiles.Size = new Size(48, 17);
    radioUnitsMiles.TabIndex = 12;
    radioUnitsMiles.TabStop = true;
    radioUnitsMiles.Text = "miles";
    radioUnitsMiles.UseVisualStyleBackColor = true;
    // 
    // groupUnits
    // 
    groupUnits.Controls.Add(radioUnitsMiles);
    groupUnits.Controls.Add(radioUnitsKM);
    groupUnits.Location = new Point(212, 202);
    groupUnits.Name = "groupUnits";
    groupUnits.Size = new Size(59, 73);
    groupUnits.TabIndex = 13;
    groupUnits.TabStop = false;
    groupUnits.Text = "units";
    // 
    // DistanceForm
    // 
    AcceptButton = btnCalc;
    AutoScaleDimensions = new SizeF(6F, 13F);
    AutoScaleMode = AutoScaleMode.Font;
    ClientSize = new Size(484, 304);
    Controls.Add(groupUnits);
    Controls.Add(btnCalc);
    Controls.Add(lblDist);
    Controls.Add(lblLong2);
    Controls.Add(lblLatt2);
    Controls.Add(lblLong1);
    Controls.Add(lblLatt1);
    Controls.Add(tboxDist);
    Controls.Add(tboxLong2);
    Controls.Add(tboxLatt2);
    Controls.Add(tboxLong1);
    Controls.Add(tboxLatt1);
    Name = "DistanceForm";
    Text = "Distance";
    groupUnits.ResumeLayout(false);
    groupUnits.PerformLayout();
    ResumeLayout(false);
    PerformLayout();
  }

  #endregion

  private TextBox tboxLatt1;
  private TextBox tboxLong1;
  private TextBox tboxLatt2;
  private TextBox tboxLong2;
  private TextBox tboxDist;
  private Label lblLatt1;
  private Label lblLong1;
  private Label lblLatt2;
  private Label lblLong2;
  private Label lblDist;
  private Button btnCalc;
  private RadioButton radioUnitsKM;
  private RadioButton radioUnitsMiles;
  private GroupBox groupUnits;
}
