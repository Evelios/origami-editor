using Godot;
using GuiLibrary;

public class FileMenu : FileMenuFs
{
	[Signal]
	public delegate void CreateNewFile();

	public override void NewFile()
	{
		GD.Print(nameof(CreateNewFile));
		EmitSignal(nameof(CreateNewFile));
	}
}
