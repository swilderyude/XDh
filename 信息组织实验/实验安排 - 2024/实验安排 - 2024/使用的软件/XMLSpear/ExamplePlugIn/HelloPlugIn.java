/*
 * Copyright DonkeyDevelopment 2006
 */

/*
 * to use the sample plugin you have to export to a jar
 * and add the jar to the classpath and pluginpath in  XMLSpear.properties
 * to compile the classes you need the donkey2_out.jar which is in the lib of XMLSpear
 */
import java.awt.event.ActionEvent;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.net.URL;

import javax.swing.AbstractAction;
import javax.swing.ImageIcon;
import javax.swing.JOptionPane;

import com.dd.gui.components.Frame;
import com.dd.gui.popup.ChooseYourEditorPopup;
import com.dd.gui.popup.ChooseYourEditorPopupVO;
import com.dd.handler.Adapter;
import com.dd.handler.ContentHandler;
import com.dd.handler.PlugInManager;
import com.dd.sources.Source;

public class HelloPlugIn extends Adapter
{
	public static String provider = "DonkeyDevelopment";

	public final static String name = "Hello PlugIn";

	public final static String version = "1.00";

	public final static String versionDate = "2006/12/05";

	public HelloPlugIn(Frame frame)
	{
		super(frame);
	}

	public AbstractAction[] getImportActions()
	{
		return new AbstractAction[] { new BuildAction() };
	}

	/*
	 * Give the actions to be showed in import menu
	 */
	public AbstractAction[] getExportActions()
	{
		return new AbstractAction[] { new SerializeAction() };
	}

	public AbstractAction[] getProjectTreeActions(Object object)
	{
		if (object instanceof File)
		{
			File file = (File) object;
			return new AbstractAction[] { new FileSizeAction(file) };
		}
		return null;
	}

	/*
	 * Give the actions to be showed in export menu
	 */
	class BuildAction extends AbstractAction
	{
		public BuildAction()
		{
			super("Import name", new ImageIcon(HelloPlugIn.class
					.getResource("import.gif")));
			putValue(AbstractAction.SHORT_DESCRIPTION,
					"Import your name in xml");
		}

		public void actionPerformed(ActionEvent event)
		{
			String name = (String) JOptionPane.showInputDialog(Frame
					.getInstance(), "Give your name", "Name Dialog",
					JOptionPane.PLAIN_MESSAGE, null, null, null);
			// do something
			// and hand it over to a handler
			PlugInManager contentManager = PlugInManager.getInstance();
			ContentHandler contentHandler = contentManager
					.getContentHandler("xml");
			if (contentHandler != null)
			{
				Source source = contentHandler.getSource();
				source.setName("hallo");
				source.setContents("<hello>" + name + "</hello>");

				// you can also give the XML as DOM
				// then you have to cast the source to a XMLSource
				// please only use the methods setDocument and
				// setSchemaValidationType
				// the other methods ar not yet officially released

				// XMLSource xmlSource = (XMLSource) source;
				// Document doc =
				// xmlSource.setDocument(doc);
				// and maybe set the validiation type
				// xmlSource.setSchemaValidationType(SchemaSource.SCHEMA_TYPE_XSD);

				// there is only one instance of the plugin
				// so you can not use member variables to save
				// information specific to a source that you need
				// later in the serialize actions
				// Instead save this in the source itself by
				// using the supplied hashtable
				source.addProperty("key1", "this is important info");
				contentHandler.open(source);
			}
		}

		/*
		 * Exaxmple where you hand over a file to the contentHandler Rename the
		 * method to actionPerformed to use it.
		 */
		public void actionPerformedFile(ActionEvent event)
		{
			try
			{
				File temp;
				temp = File.createTempFile("name", ".xml");
				temp.deleteOnExit();
				writeTextToFile(temp, "<hello>" + name + "</hello>", "UTF-8");
				PlugInManager contentManager = PlugInManager.getInstance();
				ContentHandler contentHandler = contentManager
						.getContentHandler("xml");
				if (contentHandler != null)
				{
					// default the name of the file will be used in the tab
					contentHandler.open(temp);
					Source source = Frame.getInstance().getActiveSource();
					if (source != null)
					{
						// there is only one instance of the plugin
						// so you can not use member variables to save
						// information specific to a source that you need
						// later in the serialize actions
						// Instead save this in the source itself by
						// using the supplied hashtable
						source.addProperty("key1", "this is important info");
					}
				}
			}
			catch (IOException e)
			{
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}

		/*
		 * Example to let the user decide for a contentHanler
		 */
		public ContentHandler askContentHandler()
		{
			ChooseYourEditorPopupVO answer = new ChooseYourEditorPopupVO();
			answer.setPlugInListList(PlugInManager.getInstance()
					.getContentHandlers());
			ChooseYourEditorPopup popup = new ChooseYourEditorPopup(null,
					"Which ContentHandler", true, answer);
			popup.showDialog();
			if (answer.isActionOk())
			{
				return answer.getSelectedContentHandler();
			}
			return null;
		}
	}

	class SerializeAction extends AbstractAction
	{
		public SerializeAction()
		{
			super("Export your name", new ImageIcon(HelloPlugIn.class
					.getResource("export.gif")));
			putValue(AbstractAction.SHORT_DESCRIPTION, "Export your name");
		}

		public boolean isEnabled()
		{
			// System.out.println("Serialize isEnabled");
			if (Frame.getInstance().getActiveSource() != null)
			{
				// System.out.println("Serialize isEnabled is true");
				return true;
			}
			// System.out.println("Serialize isEnabled is false");
			return false;
		}

		public void actionPerformed(ActionEvent event)
		{
			System.out.println("serialize action");
			try
			{
				if (Frame.getInstance().getComponentManager() != null)
				{
					Source source = Frame.getInstance().getActiveSource();
					System.out.println((String) source.getProperty("key1"));
					String contents = source.getContents();
					JOptionPane
							.showMessageDialog(Frame.getInstance(), contents);

					/*
					 * you can also cast to the give source
					 * and use the getContentsAsDOM()
					 * the other methods ar not yet officially released
					 * 
					 * Document doc = null; 
					 * if (Frame.getInstance().getComponentManager() != null)
					 * {
					 *   Source source = Frame.getInstance().getActiveSource();
					 *   System.out.println((String) source.getProperty("key1"));
					 *   if (source instanceof XMLSource)
					 *     {
					 * 
					 *       try { doc = ((XMLSource)source).getContentsAsDOM(); }
					 * catch (IOException e) { // TODO Auto-generated catch
					 * block e.printStackTrace(); } }
					 *  } serializeDocument(doc);
					 * 
					 */

				}
			}
			catch (IOException e)
			{
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
	}

	/*
	 * Sample action for projectTree
	 */
	class FileSizeAction extends AbstractAction
	{
		File file = null;

		public FileSizeAction(File file)
		{
			super("Show file size");
			this.file = file;
			putValue(AbstractAction.SHORT_DESCRIPTION, "Show file size");
		}

		public void actionPerformed(ActionEvent event)
		{
			JOptionPane.showMessageDialog(Frame.getInstance(), "size = "
					+ file.length());
		}

		public boolean isEnabled()
		{
			if (file.isFile())
			{

				return true;
			}

			return false;
		}

	}

	private boolean writeTextToFile(File file, String content, String encoding)
	{
		boolean succes = true;
		// save text to file
		FileOutputStream fos = null;
		BufferedWriter bw = null;
		try
		{
			fos = new FileOutputStream(file);
			bw = new BufferedWriter(new OutputStreamWriter(fos, encoding));
			bw.write(content);
		}
		catch (IOException e)
		{
			e.printStackTrace();
			succes = false;
		}
		finally
		{
			if (bw != null)
				try
				{
					bw.close();
				}
				catch (Exception ex)
				{
				}
			if (fos != null)
				try
				{
					fos.close();
				}
				catch (Exception ex)
				{
				}
		}
		return succes;
	}

	/**
	 * @return Returns the name.
	 */
	public String getName()
	{
		return name;
	}

	/**
	 * @return Returns the provider.
	 */
	public String getProvider()
	{
		return provider;
	}

	/**
	 * @return Returns the version.
	 */
	public String getVersion()
	{
		return version;
	}

	/**
	 * @return Returns the versionDate.
	 */
	public String getVersionDate()
	{
		return versionDate;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.dd.handler.ContentHandler#getMoreInfo()
	 */
	public URL getMoreInfo()
	{
		// TODO Auto-generated method stub
		return HelloPlugIn.class.getResource("aboutHello.html");
	}
}