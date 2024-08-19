using static System.Math;

namespace Geometries;

public partial class DistanceForm : Form {
  #region Properties
  public Double UnitDistance { get; set; }
  #endregion

  #region Constructors
  public DistanceForm() {
    InitializeComponent();
  }
  #endregion

  #region Methods
  private void btnCalc_Click(object sender, EventArgs e) {
    //
    // City     Latt    Long
    // ----     ----    ----
    // NYC      40.77   74
    // SF       37.75   122.68
    // Oakland  37.73   122.22
    // Burbank  34.2    118.37
    //
    // Kailash  31.067  81.3125
    // Meru	    30.868  79.0322   135.8 miles
    //
    Double dLattDeg1 = 0, dLongDeg1 = 0, dLattDeg2 = 0, dLongDeg2 = 0;

    var bParsed =
      Double.TryParse(tboxLatt1.Text, out dLattDeg1) &&
      Double.TryParse(tboxLong1.Text, out dLongDeg1) &&
      Double.TryParse(tboxLatt2.Text, out dLattDeg2) &&
      Double.TryParse(tboxLong2.Text, out dLongDeg2);

    if (bParsed) {
      UnitDistance = distGreatCircleDeg(
        dLattDeg1, dLongDeg1,
        dLattDeg2, dLongDeg2);
      updateDistance(UnitDistance);
    }
  }

  private void updateDistance(Double dUnitDistance) {
    const Double
      dMetersPerMile = 2.54 * 12 * 5280 / 100,
      dEarthRadiusKM = 6371.0008;         // Volumic Radius of the Earth in km
    var dEarthRadius = dEarthRadiusKM;

    if (radioUnitsMiles.Checked)
      dEarthRadius *= 1000 / dMetersPerMile;

    tboxDist.Text = String.Format("{0:0.0000}", dEarthRadius * dUnitDistance);
  }

  public Double distGreatCircleDeg(
    Double dLattDeg1, Double dLongDeg1,
    Double dLattDeg2, Double dLongDeg2) {
    const Double dRadPerDeg = PI / 180;
    return distGreatCircleRad(
      dRadPerDeg * dLattDeg1, dRadPerDeg * dLongDeg1,
      dRadPerDeg * dLattDeg2, dRadPerDeg * dLongDeg2);
  }

  public Double distGreatCircleRad(
    Double dLattRad1, Double dLongRad1,
    Double dLattRad2, Double dLongRad2) {
    // The following returns a result known as the Spherical Law of Cosines:
    var dProduct =
      Sin(dLattRad1) * Sin(dLattRad2) +
      Cos(dLattRad1) * Cos(dLattRad2) * Cos(dLongRad1 - dLongRad2);
    return Acos(dProduct);
  }

  private void radioUnitsKM_CheckedChanged(object sender, EventArgs e) {
    updateDistance(UnitDistance);
  }
  #endregion
}
