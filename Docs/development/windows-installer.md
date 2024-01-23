## Prerequisites
You must have a windows dev kit installed and on your `Path` so you have access to the `MakeAppx`, `SignTool`, etc.
If you can build JASP this is likely already the case.
You can quickly verify this by running `SignTool` in powershell or cmd.

## Creating your own codesigning certificate
Microsoft provides an extensive guide on how to create a password protected codesigning certificate for MSIX testing [here](https://learn.microsoft.com/en-us/windows/msix/package/create-certificate-package-signing).

## Configuring CMAKE
Provide the 'Subject' property of your generated certificate to cmake by setting it in the `MSIX_NIGHTLY_PUBLISHER` variable.

Provide the path to the certificate file by setting the `MSIX_SIGN_CERT_PATH` variable.

Provide the password in the `MSIX_SIGN_CERT_PASSWORD` variable.

The generated MSIX nightly in will be automatically signed with your certificate.
The generated MSIX for the Microsoft Store will be unsigned and must be provided to Microsoft this way.

## Generating the MSIX
The MSIXs can be generated after building JASP by running the following commands in your JASP build folder:

```
cmake --build . --target windowsPreInstallHacks
cmake --build . --target install
cmake --build . --target collect-junctions
cmake --install . --component MSIX
cmake --build . --target msix
```

This is subject to change, for an up to date sequence take a peak at the [buildbot-script](Tools/windows/BuildBotScript.cmd).
