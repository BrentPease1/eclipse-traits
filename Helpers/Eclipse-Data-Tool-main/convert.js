document
  .getElementById("csvFileInput")
  .addEventListener("change", handleFileSelect, false);

// Besselian Elements for Solar Eclipses
const eclipse_elements = {
  "08-21-2017": [
    2457987.268521, 18.0, -4.0, 4.0, 70.3, 70.3, -0.129571, 0.5406426, -2.94e-5,
    -8.1e-6, 0.485416, -0.14164, -9.05e-5, 2.05e-6, 11.8669596, -0.013622,
    -2.0e-6, 89.24543, 15.0039368, 0.0, 0.542093, 0.0001241, -1.18e-5,
    -0.004025, 0.0001234, -1.17e-5, 0.0046222, 0.0045992,
  ],
  "10-14-2023": [
    2460232.25047, 18.0, -4.0, 4.0, 73.7, 73.7, 0.169658, 0.4585533, 2.78e-5,
    -5.43e-6, 0.334859, -0.2413671, 2.4e-5, 3.03e-6, -8.2441902, -0.014888,
    2.0e-6, 93.5017319, 15.0035286, 0.0, 0.564311, -0.0000891, -1.03e-5,
    0.018083, -0.0000886, -1.03e-5, 0.0046882, 0.0046648,
  ],
  "04-08-2024": [
    2460409.26284, 18.0, -4.0, 4.0, 74.0, 74.0, -0.318244, 0.5117116, 3.26e-5,
    -8.42e-6, 0.219764, 0.2709589, -5.95e-5, -4.66e-6, 7.5862002, 0.014844,
    -2.0e-6, 89.591217, 15.0040817, 0.0, 0.535814, 0.0000618, -1.28e-5,
    -0.010272, 0.0000615, -1.27e-5, 0.0046683, 0.004645,
  ],
};

eclipse_date_select = document.getElementById("eclipseDateSelect");

// Add supported eclipse dates from above to selection
Object.keys(eclipse_elements).forEach(function (date) {
  var option = document.createElement("option");
  option.value = date;
  option.innerHTML = date;
  eclipse_date_select.appendChild(option);
});

function handleFileSelect(event) {
  const file = event.target.files[0];

  if (file) {
    const reader = new FileReader();

    reader.onload = function (e) {
      const content = e.target.result;
      handleFileUpload(content);
    };

    reader.readAsText(file);
  }
}

function handleFileUpload(csvContent) {
  try {
    if (document.getElementById("coordinatesRadio").checked) {
      const coordinates = parseCoordinateCSV(csvContent);
      processCoordinates(coordinates);
    } else {
      zipCodes = parseZipCodeCSV(csvContent);
      getCoordinatesForZipCodes(zipCodes).then((coordinates) => {
        processCoordinates(coordinates);
      });
    }
  } catch (error) {
    alert(`Error: ${error.message}`);
    return;
  }
}

function parseCoordinateCSV(csvContent) {
  const lines = csvContent.split("\n");
  const header = lines[0].split(",");
  const coordinates = [];

  if (header.length != 2) {
    throw Error(`Expected CSV header to only have two headers (Latitude, Longitude) but found ${header.length}.`)
  }

  for (let i = 1; i < lines.length; i++) {
    const data = lines[i].split(",");

    if (data.length === header.length) {

      if (data[0].trim().length == 0 && data[1].trim().length == 0) {
        // Skip empty entries
        continue;
      }

      const coordinate = {
        latitude: parseFloat(data[0]),
        longitude: parseFloat(data[1]),
        zipCode: "",
      };

      coordinates.push(coordinate);
    } else {
      throw Error(`The CSV has missing data on line ${i}.`)
    }
  }

  return coordinates;
}

function parseZipCodeCSV(csvContent) {
  const lines = csvContent.split("\n");
  const header = lines[0].split(",");

  if (header.length != 1) {
    throw Error(`Expected CSV to only have a single header (ZipCode) but found ${header.length}.`)
  }

  const zipCodes = [];

  for (let i = 1; i < lines.length; i++) {
    const data = lines[i].split(",");

    if (data.length >= 1) {
      if (data[0].trim().length == 0) {
        // Skip empty entries
        continue;
      }

      if (data[0].length < 5) {
        throw Error("Zip Codes less than 5 digits are not supported.");
      }

      zipCodes.push(data[0]);
    } else {
      throw Error("CSV file contains an empty entry.");
    }
  }

  return zipCodes;
}

async function getCoordinatesForZipCodes(zipCodes) {
  const coordinates = [];

  for (const zipCode of zipCodes) {
    const coordinate = await retrieveCoordinateFromZipCode(zipCode);
    coordinates.push(coordinate);
  }

  return coordinates;
}

async function retrieveCoordinateFromZipCode(zipCode) {
  const apiKey = "API_KEY";
  const apiUrl = `https://maps.googleapis.com/maps/api/geocode/json?components=country:US|postal_code:${zipCode}&key=${apiKey}`;

  try {
    const response = await fetch(apiUrl);
    if (!response.ok) {
      throw new Error(`HTTP error! Status: ${response.status}`);
    }

    const data = await response.json();
    if (data.status === "OK" && data.results.length > 0) {
      const location = data.results[0].geometry.location;
      return {
        latitude: location.lat,
        longitude: location.lng,
        zipCode: zipCode,
      };
    } else {
      throw new Error("Invalid response from Google Geolocation API.");
    }
  } catch (error) {
    const errorMessage = `Error: ${error.message}`;
    alert(errorMessage);
    throw new Error(errorMessage);
  }
}

function processCoordinates(coordinates) {
  const processedData = [];

  coordinates.forEach(function (coordinate) {
    selected_date = document.getElementById("eclipseDateSelect").value;
    const elements = eclipse_elements[selected_date];

    calculate(
      elements,
      (latitude = coordinate["latitude"]),
      (longitude = coordinate["longitude"])
    );

    var eclipseType = "None";
    switch (gettype()) {
      case 0:
        eclipseType = "None";
        break;
      case 1:
        eclipseType = "Partial";
        break;
      case 2:
        eclipseType = "Annular";
        break;
      case 3:
        eclipseType = "Total";
        break;
    }

    const processedResult = {
      type: eclipseType,
      coverage: getcoverage().toFixed(2),
      firstContactDate: getC1Date(elements),
      firstContactTime: getC1Time(elements),
      secondContactTime: validateTime(getC2Time(elements)),
      thirdContactTime: validateTime(getC3Time(elements)),
      fourthContactTime: getC4Time(elements),
      totalEclipseTime: getMidTime(elements),
    };

    // Add processed data to the array
    processedData.push({ ...coordinate, ...processedResult });
  });

  // Create a new CSV file with processed data
  createProcessedCSV(processedData);
}

function createProcessedCSV(data) {
  const header = [
    "ZipCode",
    "Latitude",
    "Longitude",
    "LocalType",
    "CoveragePercent",
    "FirstContactDate",
    "FirstContactTimeUTC",
    "SecondContactTimeUTC",
    "ThirdContactTimeUTC",
    "FourthContactTimeUTC",
    "TotalEclipseTimeUTC",
  ];

  const outputCSV = data.map((item) => [
    item.zipCode,
    item.latitude,
    item.longitude,
    item.type,
    item.coverage,
    item.firstContactDate,
    item.firstContactTime,
    item.secondContactTime,
    item.thirdContactTime,
    item.fourthContactTime,
    item.totalEclipseTime,
  ]);

  outputCSV.unshift(header);

  const csvContent = outputCSV.map((row) => row.join(",")).join("\n");

  // Create a Blob and set it as the href for the download link
  const blob = new Blob([csvContent], { type: "text/csv" });
  const url = URL.createObjectURL(blob);
  const downloadLink = document.getElementById("downloadLink");
  downloadLink.href = url;
  downloadLink.download = "eclipse_data.csv";
  downloadLink.style.display = "block";
}

function validateTime(val) {
  if (val.indexOf('NaN') > -1) {
    return 0;
  }

  return val;
}
