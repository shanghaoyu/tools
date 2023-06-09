# converter, from binary file to ordinary file.
import numpy as np

# Usage example
binary_file = "matrixnum.bin"  # 二进制文件名
output_file = "matrixnum.dat"  # 输出的 .dat 文件名


def convert_binary_to_dat(binary_file, output_file):
    with open(binary_file, "rb") as fp:
        # 从二进制文件中读取数据
        NMesh = int.from_bytes(fp.read(4), byteorder="little", signed=True)
        Jmax = int.from_bytes(fp.read(4), byteorder="little", signed=True)
        NChan = int.from_bytes(fp.read(4), byteorder="little", signed=True)
        mesh_points = np.frombuffer(fp.read(8 * NMesh), count=NMesh, dtype=np.float64)
        mesh_weights = np.frombuffer(fp.read(8 * NMesh), count=NMesh, dtype=np.float64)

        with open(output_file, "w") as outfile:
            outfile.write(f"NMesh:\n{NMesh}\n")
            outfile.write(f"Jmax:\n{Jmax}\n")
            outfile.write(f"NChan:\n{NChan}\n")
            outfile.write("Momentum Mesh Points:\n")
            np.savetxt(outfile, mesh_points, fmt="%.17f")
            outfile.write("Momentum Mesh Weights:\n")
            np.savetxt(outfile, mesh_weights, fmt="%.17f")

            for _ in range(NChan):
                J_read = int.from_bytes(fp.read(4), byteorder="little", signed=True)
                Prty_read = int.from_bytes(fp.read(4), byteorder="little", signed=True)
                S_read = int.from_bytes(fp.read(4), byteorder="little", signed=True)
                Tz_read = int.from_bytes(fp.read(4), byteorder="little", signed=True)
                Ndim = int.from_bytes(fp.read(4), byteorder="little", signed=True)

                V = np.frombuffer(
                    fp.read(8 * Ndim * Ndim), count=Ndim * Ndim, dtype=np.float64
                )
                V = np.reshape(V, (Ndim, Ndim))

                outfile.write(f"J:\n{J_read}\n")
                outfile.write(f"Prty:\n{Prty_read}\n")
                outfile.write(f"S:\n{S_read}\n")
                outfile.write(f"Tz:\n{Tz_read}\n")
                outfile.write(f"Ndim:\n{Ndim}\n")
                outfile.write("V:\n")
                np.savetxt(outfile, V, fmt="%.17f")

    print(f"Conversion complete. The data has been saved to {output_file}.")


if __name__ == "__main__":
    convert_binary_to_dat(binary_file, output_file)
