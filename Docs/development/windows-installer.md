## Prerequisites
You must have a windows dev kit installed and on your `Path` so you have access to the `MakeAppx`, `SignTool`, etc.
If you can build JASP this is likely already the case.
You can quickly verify this by running `SignTool` in powershell or cmd.

## Creating your own codesigning certificate
To build MSIXes and be able to install them you need a code-signing key.
An easy way to do this is by creating a self-signed certificate. 
This way you can test your own builds, to distribute it you would need to share your key with others.
Impractical to say the least, so we do not use this for releases, but it is very useful for development.

Create a certificate in a powershell:
```
New-SelfSignedCertificate -Type Custom -Subject "CN=JASP, O=CopyPaste JaspDev, C=US" -KeyUsage DigitalSignature -FriendlyName "Jasp by Moi" -CertStoreLocation "Cert:\CurrentUser\My"  -TextExtension @("2.5.29.37={text}1.3.6.1.5.5.7.3.3", "2.5.29.19={text}")
```

This will print a thumbprint, you will need it for the next command, where we export the certificate to a pfx file.
Come up with a password of some kind and run the following:

```
$password = ConvertTo-SecureString -String <Your Password> -Force -AsPlainText 
Export-PfxCertificate -cert "Cert:\CurrentUser\My\<Certificate Thumbprint>" -FilePath <FilePath>.pfx -Password $password
```

If you have more questions, or you really like reading documentation, we refer you to [Microsoft's extensive guide](https://learn.microsoft.com/en-us/windows/msix/package/create-certificate-package-signing) on how to create a password protected codesigning certificate for MSIX testing.

## Importing codesigning certificate
The above key needs to be imported as a "Trusted Root Certificate Authority" to actually install the nightly msix.
So you or anyone you share the msix with will need to:
- Right click the pfx file from above
- follow [these steps](./msix-nightly-testing.md) entering the password youve created above when asked.

## Configuring CMAKE
Provide the 'Subject' property of your generated certificate to cmake by setting it in the `MSIX_NIGHTLY_PUBLISHER` variable.
(This would be "CN=JASP, O=CopyPaste JaspDev, C=US" for the example)

Provide the path to the certificate file by setting the `MSIX_SIGN_CERT_PATH` variable.

Provide the password in the `MSIX_SIGN_CERT_PASSWORD` variable.

The generated MSIX nightly within the JASP folder in your build folder will automatically be signed with your certificate.
The generated MSIX for the Microsoft Store will be unsigned and must be provided to Microsoft this way.

## Generating the MSIX
The MSIXs can be generated after building JASP by running the following commands in your JASP build folder:

```
cmake --build . --target install && cmake --build . --target collect-junctions && cmake --install . --component MSIX && cmake --build . --target msix
```

This is subject to change, for an up to date sequence take a peak at the [buildbot-script](/Tools/windows/BuildBotScript.cmd).
