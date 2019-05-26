library("sodium", "base64enc")

# *** Functions
# Write to file, with optional base64 encoding
write_file = function(data, filename, base64 = FALSE) { 
  if (base64) {
    data = base64enc::base64encode(data)
  }
  writeChar(data, filename) 
}
# Read from file, with option base64 encoding
read_file = function(filename, base64 = FALSE)  { 
  data = readChar(filename, file.info(filename)$size, useBytes = TRUE)
  if (base64) {
    data = base64enc::base64decode(data)
  }
  return (data)
}
# Encrypt using base64 encoding as intermediate steps to force text.
# Otherwise, some kind of binary artefacts (related to R's variable typing?)
# gets included in encryption/decryption output
encrypt_string = function (decrypted, public_key) {
  decrypted = charToRaw(decrypted)
  encrypted = sodium::simple_encrypt(decrypted, public_key)
  return (encrypted)
}
# Decrypt via a same base64 decoding steps
decrypt_string = function (encrypted, private_key) {
  decrypted = sodium::simple_decrypt(encrypted, secret_key)
  decrypted = rawToChar(decrypted)
  return (decrypted)
}

# *** Algorithm
# Decide what to do
while (TRUE) {
  action = readline("g = generate keys, e = encrypt, d = decrypt> ");
  if (action == "g") {
    # Generate keys and store to file
    keypair = sodium::keygen()
    secret_key = keypair;
    write_file(
      secret_key,
      file = paste("secret_key_base64.txt", sep = ""),
      base64 = TRUE
    )
    public_key = sodium::pubkey(keypair);
    write_file(
      public_key,
      file = paste("public_key_base64.txt", sep = ""),
      base64 = TRUE
    )
    print(paste("Saved public to key to public_key_base64.txt and secret key to secret_key_base64.txt in", getwd()))
  } else if (action == "e") {
    # Read decrypted and public_key
    decrypted = read_file(
      file = "decrypted.txt", 
      base64 = FALSE
    )
    public_key = read_file(
      "public_key_base64.txt", 
      base64 = TRUE
    )
    # Encrypt
    encrypted = encrypt_string(decrypted, public_key)
    # Save encrypted
    write_file(
      encrypted,
      file = paste("encrypted.txt", sep = ""),
      base64 = TRUE
    )      
    print(paste("Encrypted decrypted.txt using public key public_key_base64.txt and saved output to encrypted.txt in", getwd()));
  } else if (action == "d") {
    # Read encrypted and secret_key
    encrypted = read_file(
      "encrypted.txt", 
      base64 = TRUE
    )
    secret_key = read_file(
      "secret_key_base64.txt",
      base64 = TRUE
    )
    # Decrypt
    decrypted = decrypt_string(encrypted, secret_key)
    print(paste("Decrypted encrypted.txt using secret key secret_key_base64.txt. The output is:", decrypted))
  }
}
