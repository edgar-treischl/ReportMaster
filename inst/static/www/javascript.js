document.addEventListener('DOMContentLoaded', function() {
  const imageSelect = document.getElementById('image_select');
  const selectedImage = document.getElementById('selected-image');
  const cardHeader = document.querySelector('.card-header'); // Select the header dynamically

  // Add event listener for dropdown change
  imageSelect.addEventListener('change', function() {
    // Get the selected value from dropdown (it should be the plot name without the extension)
    const selectedImagePath = this.value;

    // Dynamically build the image source path using JavaScript variables passed from R
    const imagePath = basePath + snr + "_" + year + "/" + plotsDir + "/" + selectedImagePath + ".png";

    // Update the image source dynamically based on the selected plot name
    selectedImage.src = imagePath;

    // Update the card header with the new title (image name)
    cardHeader.innerHTML = "Abbildung: " + selectedImagePath;

    // Optional: Log the selected path to console for debugging
    console.log('Selected Image Path:', selectedImage.src);
  });
});
