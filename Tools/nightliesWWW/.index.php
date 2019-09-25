<!doctype html>
<html>

<head>
  <meta charset="UTF-8">
  <title>JASP Nightlies</title>
  <link rel="stylesheet" href=".style.css">
</head>

<body>

<div id="bovenkant" class="container">
  <div id="titel" >
  <img id="logo"  src="http://jasp-stats.org/wp-content/themes/jasp/images/logo.svg"></img>
    <h1>Nightly builds</h1>
  </div>
  <img id="blauw" src="http://jasp-stats.org/wp-content/themes/jasp/images/wave-down-blue.svg"></img>   
</div>

<div id="tafels" class="container">
<div id="tablegrouper">
      <?php

        function convertToArray($inputDirName)
        {
          $inputDir=openDir($inputDirName);

          while(false !== ($entryName = readdir($inputDir))) 
          {
            if($entryName !== "." && $entryName !== "..")
              $fileNameArray[filemtime("$inputDirName/$entryName")]=$entryName; 
              //$fileNameArray[]=$entryName; 
          };
          //

          closedir($inputDir);

          ksort($fileNameArray);

          foreach($fileNameArray as $modTime => $fileName)
            $indexableFileNames[]=$fileName;
          

          return array_reverse($indexableFileNames);

          //return $fileNameArray;
        }

        function createTable($name, $inputDirName)
        {
          print("
    <table class=\"tafel\">
      <thead>
      <tr>
          <th colspan=\"5\"><h2>$name</h2></th>
        </tr>

        <tr>
          <th>File</th>
          <th>Branch</th>
          <th>Size <small>MB</small></th>
          <th>Uploaded</th>
          <th>Source</th>
        </tr>
      </thead>
      <tbody>");

          // Opens directory
          

          $inputArray   = convertToArray($inputDirName);
          $indexCount   = count($inputArray);

          for($index=0; $index < $indexCount; $index++) 
          {
            $inputName="";
            $inputSize="";
            $inputTime="";
            $inputPath="";
            $inputCommit="";
            $inputBranch="?";

            date_default_timezone_set('Europe/Amsterdam');
            setlocale(LC_TIME, 'nl_NL');
            $formatTime="%H:%M %d-%m-%G";

            $inputName = $inputArray[$index];
            $inputPath = "$inputDirName/$inputName";
            $inputSize = number_format(filesize($inputPath) / 1000000);
            $inputTime = strftime($formatTime, filemtime($inputPath));
            $inputSplit = explode("-", $inputName);
            
            if(count($inputSplit) >= 4 && $inputSplit[0] === "JASP")
            {
              $inputType       = $inputSplit[1];
              $inputBranch     = $inputSplit[2];

              #If somebody puts a - in the branchname...
              for($i=3; $i < count($inputSplit) - 1; $i++)
                $inputBranch = $inputBranch . "-" . $inputSplit[$i];

              $inputEnding     = explode(".", $inputSplit[count($inputSplit) - 1]);
              $inputCommitPart = $inputEnding[0];
              $inputCommit     = "<a href='https://github.com/jasp-stats/jasp-desktop/commit/$inputCommitPart'>GitHub</a>";
              $inputExt        = $inputEnding[1];
              $inputName       = "JASP $inputExt";
            }
            
            print("
            <tr>

            <td><a href='$inputPath'>$inputName</a></td>
            <td>$inputBranch</td>
            <td>$inputSize</td>
            <td>$inputTime</td>
            <td>$inputCommit</td>

            </tr>");
          }

          print("</tbody></table>");
        }

        createTable("MacOS", "./MacOS");

        createTable("Windows 64bit", "./Windows");
        createTable("Windows 32bit", "./Windows32");
        
        
      ?>
      

	<div id="onderkant"></div>

      </div></div>
  <img id="groen" src="http://jasp-stats.org/wp-content/themes/jasp/images/wave-up-green.svg"></img>

</body>

</html>
