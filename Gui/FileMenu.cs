using Godot;
using GuiLibrary;

public class FileMenu : FileMenuFs
{
	[Signal]
	public delegate void CreateNewFile();

	public override void NewFile()
	{
		EmitSignal(nameof(CreateNewFile));
	}
}
