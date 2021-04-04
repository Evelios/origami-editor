using GuiLibrary;
using Godot;

public class FileMenu : FileMenuFs
{
	[Signal]
	public delegate void CreateNewFile();

	public override void NewFile()
	{
		EmitSignal(nameof(CreateNewFile));
	}
}
